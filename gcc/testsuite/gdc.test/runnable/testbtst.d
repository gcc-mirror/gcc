/* PERMUTE_ARGS: -O
 * https://issues.dlang.org/show_bug.cgi?id=19813
 */

struct BitArray
{
    import core.bitop : btc, bts, btr, bsf, bt;

    size_t _len;
    size_t* _ptr;
    enum bitsPerSizeT = size_t.sizeof * 8;

    static size_t lenToDim(size_t len) @nogc pure nothrow @safe
    {
        return (len + (bitsPerSizeT-1)) / bitsPerSizeT;
    }

    this(in bool[] ba) nothrow pure
    {
        length = ba.length;
        foreach (i, b; ba)
        {
            if (b)
                bts(_ptr, i);
            else
                btr(_ptr, i);
        }
    }

    @property size_t length(size_t newlen) pure nothrow @system
    {
        if (newlen != _len)
        {
            size_t olddim = lenToDim(_len);
            immutable newdim = lenToDim(newlen);

            if (newdim != olddim)
            {
                // Create a fake array so we can use D's realloc machinery
                auto b = _ptr[0 .. olddim];
                b.length = newdim;                // realloc
                _ptr = b.ptr;
            }

            _len = newlen;
        }
        return _len;
    }

    int opCmp(ref BitArray a2) const @nogc pure nothrow
    {
        const lesser = this._len < a2._len ? &this : &a2;
        immutable fullWords = lesser._len / lesser.bitsPerSizeT;
        immutable endBits = lesser._len % lesser.bitsPerSizeT;
        auto p1 = this._ptr;
        auto p2 = a2._ptr;

        foreach (i; 0 .. fullWords)
        {
            if (p1[i] != p2[i])
            {
                return p1[i] & (size_t(1) << bsf(p1[i] ^ p2[i])) ? 1 : -1;
            }
        }

        if (endBits)
        {
            immutable i = fullWords;
            immutable diff = p1[i] ^ p2[i];
            if (diff)
            {
                immutable index = bsf(diff);
                if (index < endBits)
                {
                    // This gets optimized into OPbtst, and was doing it incorrectly
                    return p1[i] & (size_t(1) << index) ? 1 : -1;
                }
            }
        }

        return -1;
    }
}

void test1()
{
    bool[] ba = [1,0,1,0,1];
    bool[] bd = [1,0,1,1,1];

    auto a = BitArray(ba);
    auto d = BitArray(bd);

    assert(a <  d);
}

/***************************************/

// https://issues.dlang.org/show_bug.cgi?id=18748

int bt_32_imm(in uint* p)
{
    enum bitnum = 1;
    return ((p[bitnum >> 5] & (1 << (bitnum & 31)))) != 0;
}

void test18748()
{
    version (linux)
    {
        import core.sys.posix.sys.mman;
        import core.sys.posix.unistd;
        // Allocate two pages.
        immutable sz = 2 * sysconf(_SC_PAGESIZE);
        auto m = mmap(null, sz, PROT_READ, MAP_PRIVATE | MAP_ANON, -1, 0);
        // Discard the higher page. It becomes unreadable.
        munmap(m + sz / 2, sz / 2);
        // Try looking at the last 4 bytes of the readable page.
        uint* p = cast(uint*) (m + sz / 2 - uint.sizeof);
        bt_32_imm(p);
        munmap(m, sz / 2); // Free the readable page.
    }
}

/***************************************/

// https://issues.dlang.org/show_bug.cgi?id=18749

ulong f(ulong* p, uint shift)
{
    return (*p >> shift) & 1;
}

ulong g(ulong* p, ulong shift)
{
    return f(p, cast(uint) shift);
}

void test18749()
{
    enum shift = uint.max + 1L;
    assert(cast(uint) shift == 0);
    ulong s = 1;
    assert(g(&s, shift));
}


/***************************************/

int main()
{
    test1();
    test18748();
    test18749();

    return 0;
}
