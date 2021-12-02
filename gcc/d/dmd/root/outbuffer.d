/**
 * An expandable buffer in which you can write text or binary data.
 *
 * Copyright: Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * Authors:   Walter Bright, http://www.digitalmars.com
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/root/outbuffer.d, root/_outbuffer.d)
 * Documentation: https://dlang.org/phobos/dmd_root_outbuffer.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/root/outbuffer.d
 */

module dmd.root.outbuffer;

import core.stdc.stdarg;
import core.stdc.stdio;
import core.stdc.string;
import dmd.root.rmem;
import dmd.root.rootobject;
import dmd.root.string;

debug
{
    debug = stomp; // flush out dangling pointer problems by stomping on unused memory
}

/**
`OutBuffer` is a write-only output stream of untyped data. It is backed up by
a contiguous array or a memory-mapped file.
*/
struct OutBuffer
{
    import dmd.root.file : FileMapping;

    // IMPORTANT: PLEASE KEEP STATE AND DESTRUCTOR IN SYNC WITH DEFINITION IN ./outbuffer.h.
    // state {
    private ubyte[] data;
    private size_t offset;
    private bool notlinehead;
    /// File mapping, if any. Use a pointer for ABI compatibility with the C++ counterpart.
    /// If the pointer is non-null the store is a memory-mapped file, otherwise the store is RAM.
    private FileMapping!ubyte* fileMapping;
    /// Whether to indent
    bool doindent;
    /// Whether to indent by 4 spaces or by tabs;
    bool spaces;
    /// Current indent level
    int level;
    // state }

    /**
    Construct from filename. Will map the file into memory (or create it anew
    if necessary) and start writing at the beginning of it.

    Params:
    filename = zero-terminated name of file to map into memory
    */
    @trusted this(const(char)* filename)
    {
        fileMapping = new FileMapping!ubyte(filename);
        data = (*fileMapping)[];
    }

    /**
    Frees resources associated automatically.
    */
    extern (C++) ~this() pure nothrow
    {
        if (fileMapping)
        {
            if (fileMapping.active)
                fileMapping.close();
            fileMapping = null;
        }
        else
        {
            debug (stomp) memset(data.ptr, 0xFF, data.length);
            mem.xfree(data.ptr);
        }
    }

    extern (C++) size_t length() const pure @nogc @safe nothrow { return offset; }

    /**********************
     * Transfer ownership of the allocated data to the caller.
     * Returns:
     *  pointer to the allocated data
     */
    extern (C++) char* extractData() pure nothrow @nogc @trusted
    {
        char* p = cast(char*)data.ptr;
        data = null;
        offset = 0;
        return p;
    }

    /**
    Releases all resources associated with `this` and resets it as an empty
    memory buffer. The config variables `notlinehead`, `doindent` etc. are
    not changed.
    */
    extern (C++) void destroy() pure nothrow @trusted
    {
        if (fileMapping && fileMapping.active)
        {
            fileMapping.close();
            data = null;
            offset = 0;
        }
        else
        {
            debug (stomp) memset(data.ptr, 0xFF, data.length);
            mem.xfree(extractData());
        }
    }

    /**
    Reserves `nbytes` bytes of additional memory (or file space) in advance.
    The resulting capacity is at least the previous length plus `nbytes`.

    Params:
    nbytes = the number of additional bytes to reserve
    */
    extern (C++) void reserve(size_t nbytes) pure nothrow
    {
        //debug (stomp) printf("OutBuffer::reserve: size = %lld, offset = %lld, nbytes = %lld\n", data.length, offset, nbytes);
        const minSize = offset + nbytes;
        if (data.length >= minSize)
            return;

        /* Increase by factor of 1.5; round up to 16 bytes.
            * The odd formulation is so it will map onto single x86 LEA instruction.
            */
        const size = ((minSize * 3 + 30) / 2) & ~15;

        if (fileMapping && fileMapping.active)
        {
            fileMapping.resize(size);
            data = (*fileMapping)[];
        }
        else
        {
            debug (stomp)
            {
                auto p = cast(ubyte*)mem.xmalloc(size);
                memcpy(p, data.ptr, offset);
                memset(data.ptr, 0xFF, data.length);  // stomp old location
                mem.xfree(data.ptr);
                memset(p + offset, 0xff, size - offset); // stomp unused data
            }
            else
            {
                auto p = cast(ubyte*)mem.xrealloc(data.ptr, size);
                if (mem.isGCEnabled) // clear currently unused data to avoid false pointers
                    memset(p + offset + nbytes, 0xff, size - offset - nbytes);
            }
            data = p[0 .. size];
        }
    }

    /************************
     * Shrink the size of the data to `size`.
     * Params:
     *  size = new size of data, must be <= `.length`
     */
    extern (C++) void setsize(size_t size) pure nothrow @nogc @safe
    {
        assert(size <= offset);
        offset = size;
    }

    extern (C++) void reset() pure nothrow @nogc @safe
    {
        offset = 0;
    }

    private void indent() pure nothrow
    {
        if (level)
        {
            const indentLevel = spaces ? level * 4 : level;
            reserve(indentLevel);
            data[offset .. offset + indentLevel] = (spaces ? ' ' : '\t');
            offset += indentLevel;
        }
        notlinehead = true;
    }

    extern (C++) void write(const(void)* data, size_t nbytes) pure nothrow
    {
        write(data[0 .. nbytes]);
    }

    void write(const(void)[] buf) pure nothrow
    {
        if (doindent && !notlinehead)
            indent();
        reserve(buf.length);
        memcpy(this.data.ptr + offset, buf.ptr, buf.length);
        offset += buf.length;
    }

    extern (C++) void writestring(const(char)* string) pure nothrow
    {
        write(string.toDString);
    }

    void writestring(const(char)[] s) pure nothrow
    {
        write(s);
    }

    void writestring(string s) pure nothrow
    {
        write(s);
    }

    void writestringln(const(char)[] s) pure nothrow
    {
        writestring(s);
        writenl();
    }

    extern (C++) void prependstring(const(char)* string) pure nothrow
    {
        size_t len = strlen(string);
        reserve(len);
        memmove(data.ptr + len, data.ptr, offset);
        memcpy(data.ptr, string, len);
        offset += len;
    }

    /// write newline
    extern (C++) void writenl() pure nothrow
    {
        version (Windows)
        {
            writeword(0x0A0D); // newline is CR,LF on Microsoft OS's
        }
        else
        {
            writeByte('\n');
        }
        if (doindent)
            notlinehead = false;
    }

    extern (C++) void writeByte(uint b) pure nothrow
    {
        if (doindent && !notlinehead && b != '\n')
            indent();
        reserve(1);
        this.data[offset] = cast(ubyte)b;
        offset++;
    }

    extern (C++) void writeUTF8(uint b) pure nothrow
    {
        reserve(6);
        if (b <= 0x7F)
        {
            this.data[offset] = cast(ubyte)b;
            offset++;
        }
        else if (b <= 0x7FF)
        {
            this.data[offset + 0] = cast(ubyte)((b >> 6) | 0xC0);
            this.data[offset + 1] = cast(ubyte)((b & 0x3F) | 0x80);
            offset += 2;
        }
        else if (b <= 0xFFFF)
        {
            this.data[offset + 0] = cast(ubyte)((b >> 12) | 0xE0);
            this.data[offset + 1] = cast(ubyte)(((b >> 6) & 0x3F) | 0x80);
            this.data[offset + 2] = cast(ubyte)((b & 0x3F) | 0x80);
            offset += 3;
        }
        else if (b <= 0x1FFFFF)
        {
            this.data[offset + 0] = cast(ubyte)((b >> 18) | 0xF0);
            this.data[offset + 1] = cast(ubyte)(((b >> 12) & 0x3F) | 0x80);
            this.data[offset + 2] = cast(ubyte)(((b >> 6) & 0x3F) | 0x80);
            this.data[offset + 3] = cast(ubyte)((b & 0x3F) | 0x80);
            offset += 4;
        }
        else
            assert(0);
    }

    extern (C++) void prependbyte(uint b) pure nothrow
    {
        reserve(1);
        memmove(data.ptr + 1, data.ptr, offset);
        data[0] = cast(ubyte)b;
        offset++;
    }

    extern (C++) void writewchar(uint w) pure nothrow
    {
        version (Windows)
        {
            writeword(w);
        }
        else
        {
            write4(w);
        }
    }

    extern (C++) void writeword(uint w) pure nothrow
    {
        version (Windows)
        {
            uint newline = 0x0A0D;
        }
        else
        {
            uint newline = '\n';
        }
        if (doindent && !notlinehead && w != newline)
            indent();

        reserve(2);
        *cast(ushort*)(this.data.ptr + offset) = cast(ushort)w;
        offset += 2;
    }

    extern (C++) void writeUTF16(uint w) pure nothrow
    {
        reserve(4);
        if (w <= 0xFFFF)
        {
            *cast(ushort*)(this.data.ptr + offset) = cast(ushort)w;
            offset += 2;
        }
        else if (w <= 0x10FFFF)
        {
            *cast(ushort*)(this.data.ptr + offset) = cast(ushort)((w >> 10) + 0xD7C0);
            *cast(ushort*)(this.data.ptr + offset + 2) = cast(ushort)((w & 0x3FF) | 0xDC00);
            offset += 4;
        }
        else
            assert(0);
    }

    extern (C++) void write4(uint w) pure nothrow
    {
        version (Windows)
        {
            bool notnewline = w != 0x000A000D;
        }
        else
        {
            bool notnewline = true;
        }
        if (doindent && !notlinehead && notnewline)
            indent();
        reserve(4);
        *cast(uint*)(this.data.ptr + offset) = w;
        offset += 4;
    }

    extern (C++) void write(const OutBuffer* buf) pure nothrow
    {
        if (buf)
        {
            reserve(buf.offset);
            memcpy(data.ptr + offset, buf.data.ptr, buf.offset);
            offset += buf.offset;
        }
    }

    extern (C++) void write(RootObject obj) /*nothrow*/
    {
        if (obj)
        {
            writestring(obj.toChars());
        }
    }

    extern (C++) void fill0(size_t nbytes) pure nothrow
    {
        reserve(nbytes);
        memset(data.ptr + offset, 0, nbytes);
        offset += nbytes;
    }

    /**
     * Allocate space, but leave it uninitialized.
     * Params:
     *  nbytes = amount to allocate
     * Returns:
     *  slice of the allocated space to be filled in
     */
    extern (D) char[] allocate(size_t nbytes) pure nothrow
    {
        reserve(nbytes);
        offset += nbytes;
        return cast(char[])data[offset - nbytes .. offset];
    }

    extern (C++) void vprintf(const(char)* format, va_list args) nothrow
    {
        int count;
        if (doindent && !notlinehead)
            indent();
        uint psize = 128;
        for (;;)
        {
            reserve(psize);
            va_list va;
            va_copy(va, args);
            /*
                The functions vprintf(), vfprintf(), vsprintf(), vsnprintf()
                are equivalent to the functions printf(), fprintf(), sprintf(),
                snprintf(), respectively, except that they are called with a
                va_list instead of a variable number of arguments. These
                functions do not call the va_end macro. Consequently, the value
                of ap is undefined after the call. The application should call
                va_end(ap) itself afterwards.
                */
            count = vsnprintf(cast(char*)data.ptr + offset, psize, format, va);
            va_end(va);
            if (count == -1) // snn.lib and older libcmt.lib return -1 if buffer too small
                psize *= 2;
            else if (count >= psize)
                psize = count + 1;
            else
                break;
        }
        offset += count;
        if (mem.isGCEnabled)
            memset(data.ptr + offset, 0xff, psize - count);
    }

    static if (__VERSION__ < 2092)
    {
        extern (C++) void printf(const(char)* format, ...) nothrow
        {
            va_list ap;
            va_start(ap, format);
            vprintf(format, ap);
            va_end(ap);
        }
    }
    else
    {
        pragma(printf) extern (C++) void printf(const(char)* format, ...) nothrow
        {
            va_list ap;
            va_start(ap, format);
            vprintf(format, ap);
            va_end(ap);
        }
    }

    /**************************************
     * Convert `u` to a string and append it to the buffer.
     * Params:
     *  u = integral value to append
     */
    extern (C++) void print(ulong u) pure nothrow
    {
        //import core.internal.string;  // not available
        UnsignedStringBuf buf = void;
        writestring(unsignedToTempString(u, buf));
    }

    extern (C++) void bracket(char left, char right) pure nothrow
    {
        reserve(2);
        memmove(data.ptr + 1, data.ptr, offset);
        data[0] = left;
        data[offset + 1] = right;
        offset += 2;
    }

    /******************
     * Insert left at i, and right at j.
     * Return index just past right.
     */
    extern (C++) size_t bracket(size_t i, const(char)* left, size_t j, const(char)* right) pure nothrow
    {
        size_t leftlen = strlen(left);
        size_t rightlen = strlen(right);
        reserve(leftlen + rightlen);
        insert(i, left, leftlen);
        insert(j + leftlen, right, rightlen);
        return j + leftlen + rightlen;
    }

    extern (C++) void spread(size_t offset, size_t nbytes) pure nothrow
    {
        reserve(nbytes);
        memmove(data.ptr + offset + nbytes, data.ptr + offset, this.offset - offset);
        this.offset += nbytes;
    }

    /****************************************
     * Returns: offset + nbytes
     */
    extern (C++) size_t insert(size_t offset, const(void)* p, size_t nbytes) pure nothrow
    {
        spread(offset, nbytes);
        memmove(data.ptr + offset, p, nbytes);
        return offset + nbytes;
    }

    size_t insert(size_t offset, const(char)[] s) pure nothrow
    {
        return insert(offset, s.ptr, s.length);
    }

    extern (C++) void remove(size_t offset, size_t nbytes) pure nothrow @nogc
    {
        memmove(data.ptr + offset, data.ptr + offset + nbytes, this.offset - (offset + nbytes));
        this.offset -= nbytes;
    }

    /**
     * Returns:
     *   a non-owning const slice of the buffer contents
     */
    extern (D) const(char)[] opSlice() const pure nothrow @nogc @safe
    {
        return cast(const(char)[])data[0 .. offset];
    }

    extern (D) const(char)[] opSlice(size_t lwr, size_t upr) const pure nothrow @nogc @safe
    {
        return cast(const(char)[])data[lwr .. upr];
    }

    extern (D) char opIndex(size_t i) const pure nothrow @nogc @safe
    {
        return cast(char)data[i];
    }

    /***********************************
     * Extract the data as a slice and take ownership of it.
     *
     * When `true` is passed as an argument, this function behaves
     * like `dmd.utils.toDString(thisbuffer.extractChars())`.
     *
     * Params:
     *   nullTerminate = When `true`, the data will be `null` terminated.
     *                   This is useful to call C functions or store
     *                   the result in `Strings`. Defaults to `false`.
     */
    extern (D) char[] extractSlice(bool nullTerminate = false) pure nothrow
    {
        const length = offset;
        if (!nullTerminate)
            return extractData()[0 .. length];
        // There's already a terminating `'\0'`
        if (length && data[length - 1] == '\0')
            return extractData()[0 .. length - 1];
        writeByte(0);
        return extractData()[0 .. length];
    }

    // Append terminating null if necessary and get view of internal buffer
    extern (C++) char* peekChars() pure nothrow
    {
        if (!offset || data[offset - 1] != '\0')
        {
            writeByte(0);
            offset--; // allow appending more
        }
        return cast(char*)data.ptr;
    }

    // Append terminating null if necessary and take ownership of data
    extern (C++) char* extractChars() pure nothrow
    {
        if (!offset || data[offset - 1] != '\0')
            writeByte(0);
        return extractData();
    }

    /**
    Destructively saves the contents of `this` to `filename`. As an
    optimization, if the file already has identical contents with the buffer,
    no copying is done. This is because on SSD drives reading is often much
    faster than writing and because there's a high likelihood an identical
    file is written during the build process.

    Params:
    filename = the name of the file to receive the contents

    Returns: `true` iff the operation succeeded.
    */
    extern(D) bool moveToFile(const char* filename)
    {
        import dmd.root.file;
        bool result = true;
        const bool identical = this[] == FileMapping!(const ubyte)(filename)[];

        if (fileMapping && fileMapping.active)
        {
            // Defer to corresponding functions in FileMapping.
            if (identical)
            {
                result = fileMapping.discard();
            }
            else
            {
                // Resize to fit to get rid of the slack bytes at the end
                fileMapping.resize(offset);
                result = fileMapping.moveToFile(filename);
            }
            // Can't call destroy() here because the file mapping is already closed.
            data = null;
            offset = 0;
        }
        else
        {
            if (!identical)
                File.write(filename, this[]);
            destroy();
        }

        return identical
            ? result && File.touch(filename)
            : result;
    }
}

/****** copied from core.internal.string *************/

private:

alias UnsignedStringBuf = char[20];

char[] unsignedToTempString(ulong value, char[] buf, uint radix = 10) @safe pure nothrow @nogc
{
    size_t i = buf.length;
    do
    {
        if (value < radix)
        {
            ubyte x = cast(ubyte)value;
            buf[--i] = cast(char)((x < 10) ? x + '0' : x - 10 + 'a');
            break;
        }
        else
        {
            ubyte x = cast(ubyte)(value % radix);
            value = value / radix;
            buf[--i] = cast(char)((x < 10) ? x + '0' : x - 10 + 'a');
        }
    } while (value);
    return buf[i .. $];
}

/************* unit tests **************************************************/

unittest
{
    OutBuffer buf;
    buf.printf("betty");
    buf.insert(1, "xx".ptr, 2);
    buf.insert(3, "yy");
    buf.remove(4, 1);
    buf.bracket('(', ')');
    const char[] s = buf[];
    assert(s == "(bxxyetty)");
    buf.destroy();
}

unittest
{
    OutBuffer buf;
    buf.writestring("abc".ptr);
    buf.prependstring("def");
    buf.prependbyte('x');
    OutBuffer buf2;
    buf2.writestring("mmm");
    buf.write(&buf2);
    char[] s = buf.extractSlice();
    assert(s == "xdefabcmmm");
}

unittest
{
    OutBuffer buf;
    buf.writeByte('a');
    char[] s = buf.extractSlice();
    assert(s == "a");

    buf.writeByte('b');
    char[] t = buf.extractSlice();
    assert(t == "b");
}

unittest
{
    OutBuffer buf;
    char* p = buf.peekChars();
    assert(*p == 0);

    buf.writeByte('s');
    char* q = buf.peekChars();
    assert(strcmp(q, "s") == 0);
}

unittest
{
    char[10] buf;
    char[] s = unsignedToTempString(278, buf[], 10);
    assert(s == "278");

    s = unsignedToTempString(1, buf[], 10);
    assert(s == "1");

    s = unsignedToTempString(8, buf[], 2);
    assert(s == "1000");

    s = unsignedToTempString(29, buf[], 16);
    assert(s == "1d");
}
