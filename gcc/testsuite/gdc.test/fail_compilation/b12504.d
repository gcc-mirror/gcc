/*
TEST_OUTPUT:
---
fail_compilation/b12504.d(26): Error: cannot implicitly convert expression `257$?:32=u|64=LU$` of type `$?:32=uint|64=ulong$` to `ubyte`
fail_compilation/b12504.d(27): Error: index type `ubyte` cannot cover index range 0..257
fail_compilation/b12504.d(31): Error: cannot implicitly convert expression `129$?:32=u|64=LU$` of type `$?:32=uint|64=ulong$` to `byte`
fail_compilation/b12504.d(32): Error: index type `byte` cannot cover index range 0..129
fail_compilation/b12504.d(36): Error: cannot implicitly convert expression `65537$?:32=u|64=LU$` of type `$?:32=uint|64=ulong$` to `ushort`
fail_compilation/b12504.d(37): Error: index type `ushort` cannot cover index range 0..65537
fail_compilation/b12504.d(41): Error: cannot implicitly convert expression `32769$?:32=u|64=LU$` of type `$?:32=uint|64=ulong$` to `short`
fail_compilation/b12504.d(42): Error: index type `short` cannot cover index range 0..32769
fail_compilation/b12504.d(46): Error: cannot implicitly convert expression `257$?:32=u|64=LU$` of type `$?:32=uint|64=ulong$` to `ubyte`
fail_compilation/b12504.d(47): Error: index type `ubyte` cannot cover index range 0..257
fail_compilation/b12504.d(51): Error: cannot implicitly convert expression `129$?:32=u|64=LU$` of type `$?:32=uint|64=ulong$` to `byte`
fail_compilation/b12504.d(52): Error: index type `byte` cannot cover index range 0..129
fail_compilation/b12504.d(56): Error: cannot implicitly convert expression `65537$?:32=u|64=LU$` of type `$?:32=uint|64=ulong$` to `ushort`
fail_compilation/b12504.d(57): Error: index type `ushort` cannot cover index range 0..65537
fail_compilation/b12504.d(61): Error: cannot implicitly convert expression `32769$?:32=u|64=LU$` of type `$?:32=uint|64=ulong$` to `short`
fail_compilation/b12504.d(62): Error: index type `short` cannot cover index range 0..32769
---
*/
void main()
{
    {
        int[0xFF + 2] sta;
        foreach (ubyte i; 0 .. sta.length) {}
        foreach (ubyte i, x; sta) {}
    }
    {
        int[0x7F + 2] sta;
        foreach (byte i; 0 .. sta.length) {}
        foreach (byte i, x; sta) {}
    }
    {
        int[0xFFFF + 2] sta;
        foreach (ushort i; 0 .. sta.length) {}
        foreach (ushort i, x; sta) {}
    }
    {
        int[0x7FFF + 2] sta;
        foreach (short i; 0 .. sta.length) {}
        foreach (short i, x; sta) {}
    }
    {
        immutable int[0xFF + 2] sta;
        static foreach (ubyte i; 0 .. sta.length) {}
        static foreach (ubyte i, x; sta) {}
    }
    {
        immutable int[0x7F + 2] sta;
        static foreach (byte i; 0 .. sta.length) {}
        static foreach (byte i, x; sta) {}
    }
    {
        immutable int[0xFFFF + 2] sta;
        static foreach (ushort i; 0 .. sta.length) {}
        static foreach (ushort i, x; sta) {}
    }
    {
        immutable int[0x7FFF + 2] sta;
        static foreach (short i; 0 .. sta.length) {}
        static foreach (short i, x; sta) {}
    }
}
