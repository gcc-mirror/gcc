/**
 * Implementation of a bit array.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/root/bitarray.d, root/_bitarray.d)
 * Documentation:  https://dlang.org/phobos/dmd_root_array.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/root/bitarray.d
 */

module dmd.root.bitarray;

import core.stdc.stdio;
import core.stdc.string;

import dmd.root.rmem;

struct BitArray
{

    alias Chunk_t = size_t;
    enum ChunkSize = Chunk_t.sizeof;
    enum BitsPerChunk = ChunkSize * 8;

    size_t length() const @nogc nothrow pure @safe
    {
        return len;
    }

    void length(size_t nlen) nothrow pure
    {
        immutable ochunks = chunks(len);
        immutable nchunks = chunks(nlen);
        if (ochunks != nchunks)
        {
            ptr = cast(size_t*)mem.xrealloc_noscan(ptr, nchunks * ChunkSize);
        }
        if (nchunks > ochunks)
           ptr[ochunks .. nchunks] = 0;
        if (nlen & (BitsPerChunk - 1))
           ptr[nchunks - 1] &= (cast(Chunk_t)1 << (nlen & (BitsPerChunk - 1))) - 1;
        len = nlen;
    }

    void opAssign(const ref BitArray b) nothrow pure
    {
        if (!len)
            length(b.len);
        assert(len == b.len);
        memcpy(ptr, b.ptr, bytes(len));
    }

    bool opIndex(size_t idx) const @nogc nothrow pure
    {
        import core.bitop : bt;

        assert(idx < len);
        return !!bt(ptr, idx);
    }

    void opIndexAssign(bool val, size_t idx) @nogc nothrow pure
    {
        import core.bitop : btc, bts;

        assert(idx < len);
        if (val)
            bts(ptr, idx);
        else
            btc(ptr, idx);
    }

    bool opEquals(const ref BitArray b) const @nogc nothrow pure
    {
        return len == b.len && memcmp(ptr, b.ptr, bytes(len)) == 0;
    }

    void zero() @nogc nothrow pure
    {
        memset(ptr, 0, bytes(len));
    }

    /******
     * Returns:
     *  true if no bits are set
     */
    bool isZero() @nogc nothrow pure
    {
        const nchunks = chunks(len);
        foreach (i; 0 .. nchunks)
        {
            if (ptr[i])
                return false;
        }
        return true;
    }

    void or(const ref BitArray b) @nogc nothrow pure
    {
        assert(len == b.len);
        const nchunks = chunks(len);
        foreach (i; 0 .. nchunks)
            ptr[i] |= b.ptr[i];
    }

    /* Swap contents of `this` with `b`
     */
    void swap(ref BitArray b) @nogc nothrow pure
    {
        assert(len == b.len);
        const nchunks = chunks(len);
        foreach (i; 0 .. nchunks)
        {
            const chunk = ptr[i];
            ptr[i] = b.ptr[i];
            b.ptr[i] = chunk;
        }
    }

    ~this() nothrow pure
    {
        debug
        {
            // Stomp the allocated memory
            const nchunks = chunks(len);
            foreach (i; 0 .. nchunks)
            {
                ptr[i] = cast(Chunk_t)0xFEFEFEFE_FEFEFEFE;
            }
        }
        mem.xfree(ptr);
        debug
        {
            // Set to implausible values
            len = cast(size_t)0xFEFEFEFE_FEFEFEFE;
            ptr = cast(size_t*)cast(size_t)0xFEFEFEFE_FEFEFEFE;
        }
    }

private:
    size_t len;         // length in bits
    size_t *ptr;

    /// Returns: The amount of chunks used to store len bits
    static size_t chunks(const size_t len) @nogc nothrow pure @safe
    {
        return (len + BitsPerChunk - 1) / BitsPerChunk;
    }

    /// Returns: The amount of bytes used to store len bits
    static size_t bytes(const size_t len) @nogc nothrow pure @safe
    {
        return chunks(len) * ChunkSize;
    }
}

nothrow pure unittest
{
    BitArray array;
    array.length = 20;
    assert(array[19] == 0);
    array[10] = 1;
    assert(array[10] == 1);
    array[10] = 0;
    assert(array[10] == 0);
    assert(array.length == 20);

    BitArray a,b;
    assert(a != array);
    a.length = 200;
    assert(a != array);
    assert(a.isZero());
    a[100] = true;
    b.length = 200;
    b[100] = true;
    assert(a == b);

    a.length = 300;
    b.length = 300;
    assert(a == b);
    b[299] = true;
    assert(a != b);
    assert(!a.isZero());
    a.swap(b);
    assert(a[299] == true);
    assert(b[299] == false);
    a = b;
    assert(a == b);
}
