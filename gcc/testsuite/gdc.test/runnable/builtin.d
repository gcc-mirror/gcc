
import std.stdio;
import std.math;
import core.bitop;

version (DigitalMars)
{
    version (X86_64)
        version = AnyX86;
    else version (X86)
        version = AnyX86;
}

/*******************************************/

void test1()
{
    writefln("%a", sin(6.8L));
    auto f = 6.8L;
    writefln("%a", sin(f));
    assert(sin(f) == sin(6.8L));
    static assert(approxEqual(sin(6.8L), 0x1.f9f8d9aea10fdf1cp-2));

    writefln("%a", cos(6.8L));
    f = 6.8L;
    writefln("%a", cos(f));
    assert(cos(f) == cos(6.8L));
    static assert(approxEqual(cos(6.8L), 0x1.bd21aaf88dcfa13ap-1));

    writefln("%a", tan(6.8L));
    f = 6.8L;
    writefln("%a", tan(f));
    version (Win64)
    { }
    else
        assert(tan(f) == tan(6.8L));
    static assert(approxEqual(tan(6.8L), 0x1.22fd752af75cd08cp-1));
}

/*******************************************/

void test2()
{
    float i = 3;
    i = i ^^ 2;
    assert(i == 9);

    int j = 2;
    j = j ^^ 1;
    assert(j == 2);

    i = 4;
    i = i ^^ .5;
    assert(i == 2);
}

/**** Bug 5703 *****************************/

static assert({
    int a = 0x80;
    int f = bsf(a);
    int r = bsr(a);
    a = 0x22;
    assert(bsf(a)==1);
    assert(bsr(a)==5);
    a = 0x8000000;
    assert(bsf(a)==27);
    assert(bsr(a)==27);
    a = 0x13f562c0;
    assert(bsf(a) == 6);
    assert(bsr(a) == 28);
    assert(bswap(0xAABBCCDD) == 0xDDCCBBAA);
    return true;
}());

/*******************************************/

void test3()
{
    version (AnyX86)
    {
        static assert( _popcnt( cast(ushort)0 ) == 0 );
        static assert( _popcnt( cast(ushort)7 ) == 3 );
        static assert( _popcnt( cast(ushort)0xAA )== 4);
        static assert( _popcnt( cast(ushort)0xFFFF ) == 16 );
        static assert( _popcnt( cast(ushort)0xCCCC ) == 8 );
        static assert( _popcnt( cast(ushort)0x7777 ) == 12 );
        static assert( _popcnt( cast(uint)0 ) == 0 );
        static assert( _popcnt( cast(uint)7 ) == 3 );
        static assert( _popcnt( cast(uint)0xAA )== 4);
        static assert( _popcnt( cast(uint)0x8421_1248 ) == 8 );
        static assert( _popcnt( cast(uint)0xFFFF_FFFF ) == 32 );
        static assert( _popcnt( cast(uint)0xCCCC_CCCC ) == 16 );
        static assert( _popcnt( cast(uint)0x7777_7777 ) == 24 );
        version (X86_64)
        {
            static assert( _popcnt( cast(ulong)0 ) == 0 );
            static assert( _popcnt( cast(ulong)7 ) == 3 );
            static assert( _popcnt( cast(ulong)0xAA )== 4);
            static assert( _popcnt( cast(ulong)0x8421_1248 ) == 8 );
            static assert( _popcnt( cast(ulong)0xFFFF_FFFF_FFFF_FFFF ) == 64 );
            static assert( _popcnt( cast(ulong)0xCCCC_CCCC_CCCC_CCCC ) == 32 );
            static assert( _popcnt( cast(ulong)0x7777_7777_7777_7777 ) == 48 );
        }
    }
}

/*******************************************/

int main()
{
    test1();
    test2();
    test3();

    printf("Success\n");
    return 0;
}
