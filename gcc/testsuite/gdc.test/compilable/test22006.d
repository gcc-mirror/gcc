// https://issues.dlang.org/show_bug.cgi?id=22006
void test22006()
{
    alias AliasSeq(TList...) = TList;
    {
        alias aseq = AliasSeq!(0, 1, 2, 3);
        static foreach (ubyte i; 0 .. aseq.length) {}
        static foreach (ubyte i, x; aseq) {}
    }
    {
        static foreach (ubyte i; 0 .. [0, 1, 2, 3].length) {}
        static foreach (ubyte i, x; [0, 1, 2, 3]) {}
    }
}
