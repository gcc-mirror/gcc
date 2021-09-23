/*
TEST_OUTPUT:
---
fail_compilation/fail22006.d(15): Error: cannot implicitly convert expression `4$?:32=u|64=LU$` of type `$?:32=uint|64=ulong$` to `bool`
fail_compilation/fail22006.d(16): Error: index type `bool` cannot cover index range 0..4
fail_compilation/fail22006.d(19): Error: cannot implicitly convert expression `4$?:32=u|64=LU$` of type `$?:32=uint|64=ulong$` to `bool`
fail_compilation/fail22006.d(20): Error: index type `bool` cannot cover index range 0..4
---
*/
void test22006()
{
    alias AliasSeq(TList...) = TList;
    {
        alias aseq = AliasSeq!(0, 1, 2, 3);
        static foreach (bool i; 0 .. aseq.length) {}
        static foreach (bool i, x; aseq) {}
    }
    {
        static foreach (bool i; 0 .. [0, 1, 2, 3].length) {}
        static foreach (bool i, x; [0, 1, 2, 3]) {}
    }
}
