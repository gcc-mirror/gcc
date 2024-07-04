/**
 * Common string functions including filename manipulation.
 *
 * Copyright: Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:   Walter Bright, https://www.digitalmars.com
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/common/smallbuffer.d, common/_smallbuffer.d)
 * Documentation: https://dlang.org/phobos/dmd_common_smallbuffer.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/common/smallbuffer
 */
module dmd.common.smallbuffer;

nothrow:

/**
Defines a temporary array of `Element`s using a fixed-length buffer as back store. If the length
of the buffer suffices, it is readily used. Otherwise, `malloc` is used to
allocate memory for the array and `free` is used for deallocation in the
destructor.

This type is meant to use exclusively as an automatic variable. It is not
default constructible or copyable.
*/
struct SmallBuffer(Element)
{
    import core.stdc.stdlib : malloc, free;

    private Element[] _extent;
    private bool needsFree;

  nothrow:
  @nogc:

    @disable this(); // no default ctor
    @disable this(ref const SmallBuffer!Element); // noncopyable, nonassignable

    /***********
     * Construct a SmallBuffer
     * Params:
     *  len = number of elements in array
     *  buffer = slice to use as backing-store, if len will fit in it
     */
    scope this(size_t len, return scope Element[] buffer)
    {
        if (len <= buffer.length)
        {
            _extent = buffer[0 .. len];
        }
        else
        {
            assert(len < size_t.max / (2 * Element.sizeof));
            _extent = (cast(typeof(_extent.ptr)) malloc(len * Element.sizeof))[0 .. len];
            _extent.ptr || assert(0, "Out of memory.");
            needsFree = true;
        }
        assert(this.length == len);
    }

    ~this()
    {
        if (needsFree)
            free(_extent.ptr);
    }

    /******
     * Resize existing SmallBuffer.
     * Params:
     *  len = number of elements after resize
     */
    scope void create(size_t len)
    {
        if (len <= _extent.length)
        {
            _extent = _extent[0 .. len]; // reuse existing storage
        }
        else
        {
            __dtor();
            assert(len < size_t.max / Element.sizeof);
            _extent = (cast(typeof(_extent.ptr)) malloc(len * Element.sizeof))[0 .. len];
            _extent.ptr || assert(0, "Out of memory.");
            needsFree = true;
        }
        assert(this.length == len);
    }

    // Force accesses to extent to be scoped.
    scope inout extent()
    {
        return _extent;
    }

    alias extent this;
}

/// ditto
unittest
{
    char[230] buf = void;
    auto a = SmallBuffer!char(10, buf);
    assert(a[] is buf[0 .. 10]);
    auto b = SmallBuffer!char(1000, buf);
    assert(b[] !is buf[]);
    b.create(1000);
    assert(b.length == 1000);
    assert(b[] !is buf[]);
}

/**
 * (Windows only) Converts a narrow string to a wide string using `buffer` as strorage.
 * Params:
 *    narrow = string to be converted
 *    buffer = where to place the converted string
 * Returns: a slice of `buffer` containing the converted string. A zero follows the slice.
*/
version(Windows) wchar[] toWStringz(scope const(char)[] narrow, ref SmallBuffer!wchar buffer) nothrow
{
    if (narrow is null)
        return null;

    size_t charsToWchars(scope const(char)[] narrow, scope wchar[] buffer)
    {
        // https://learn.microsoft.com/en-us/windows/win32/api/stringapiset/nf-stringapiset-multibytetowidechar
        import core.sys.windows.winnls : MultiByteToWideChar, CP_ACP;
        return MultiByteToWideChar(CP_ACP, 0, narrow.ptr, cast(int) narrow.length, buffer.ptr, cast(int) buffer.length);
    }

    size_t length = charsToWchars(narrow, buffer[]);
    if (length >= buffer.length) // not enough room in buffer[]
    {
        buffer.create(length + 1); // extend buffer length
        length = charsToWchars(narrow, buffer[]);  // try again
        assert(length < buffer.length);
    }
    buffer[length] = 0;
    return buffer[0 .. length];
}

/**************************************
* Converts a path to one suitable to be passed to Win32 API
* functions that can deal with paths longer than 248
* characters then calls the supplied function on it.
*
* Params:
*  path = The Path to call F on.
*
* Returns:
*  The result of calling F on path.
*
* References:
*  https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx
*/
version(Windows) auto extendedPathThen(alias F)(const(char)[] path)
{
    import core.sys.windows.winbase;
    import core.sys.windows.winnt;

    if (!path.length)
        return F((wchar[]).init);

    wchar[1024] buf = void;
    auto store = SmallBuffer!wchar(buf.length, buf);
    auto wpath = toWStringz(path, store);

    // GetFullPathNameW expects a sized buffer to store the result in. Since we don't
    // know how large it has to be, we pass in null and get the needed buffer length
    // as the return code.
    const pathLength = GetFullPathNameW(&wpath[0],
                                        0 /*length8*/,
                                        null /*output buffer*/,
                                        null /*filePartBuffer*/);
    if (pathLength == 0)
    {
        return F((wchar[]).init);
    }

    // wpath is the UTF16 version of path, but to be able to use
    // extended paths, we need to prefix with `\\?\` and the absolute
    // path.
    static immutable prefix = `\\?\`w;

    // prefix only needed for long names and non-UNC names
    const needsPrefix = pathLength >= MAX_PATH && (wpath[0] != '\\' || wpath[1] != '\\');
    const prefixLength = needsPrefix ? prefix.length : 0;

    // +1 for the null terminator
    const bufferLength = pathLength + prefixLength + 1;

    wchar[1024] absBuf = void;
    auto absPath = SmallBuffer!wchar(bufferLength, absBuf);

    absPath[0 .. prefixLength] = prefix[0 .. prefixLength];

    const absPathRet = GetFullPathNameW(&wpath[0],
        cast(uint)(absPath.length - prefixLength - 1),
        &absPath[prefixLength],
        null /*filePartBuffer*/);

    if (absPathRet == 0 || absPathRet > absPath.length - prefixLength)
    {
        return F((wchar[]).init);
    }

    absPath[$ - 1] = '\0';
    // Strip null terminator from the slice
    return F(absPath[0 .. $ - 1]);
}
