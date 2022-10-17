// https://issues.dlang.org/show_bug.cgi?id=12504
void main()
{
    {
        int[0xFF + 1] sta;
        foreach (ubyte i; 0 .. sta.length) {}
        foreach (ubyte i, x; sta) {}
    }
    {
        int[0x7F + 1] sta;
        foreach (byte i; 0 .. sta.length) {}
        foreach (byte i, x; sta) {}
    }
    {
        int[0xFFFF + 1] sta;
        foreach (ushort i; 0 .. sta.length) {}
        foreach (ushort i, x; sta) {}
    }
    {
        int[0x7FFF + 1] sta;
        foreach (short i; 0 .. sta.length) {}
        foreach (short i, x; sta) {}
    }
    {
        immutable int[0xFF + 1] sta;
        static foreach (ubyte i; 0 .. sta.length) {}
        static foreach (ubyte i, x; sta) {}
    }
    {
        immutable int[0x7F + 1] sta;
        static foreach (byte i; 0 .. sta.length) {}
        static foreach (byte i, x; sta) {}
    }
    {
        immutable int[0xFFFF + 1] sta;
        static foreach (ushort i; 0 .. sta.length) {}
        static foreach (ushort i, x; sta) {}
    }
    {
        immutable int[0x7FFF + 1] sta;
        static foreach (short i; 0 .. sta.length) {}
        static foreach (short i, x; sta) {}
    }
}
