// https://issues.dlang.org/show_bug.cgi?id=17942

alias AliasSeq(TList...) = TList;

void test()
{
    enum A = AliasSeq!(1);
    static assert(A[0] == 1);
    static assert(B[0] == 2);
}

enum B = AliasSeq!(2);

enum C = AliasSeq!();
