// { dg-do compile }
// { dg-options "-fno-moduleinfo -fdump-tree-original" }
struct S
{
    int i;
    this(ref S);
    void opAssign(S s)
    {
        i = s.i + 1;
    }
}

void test_opAssign()
{
    S s;
    S t;
    // { dg-final { scan-tree-dump-not "&TARGET_EXPR" "original" } }
    t = s;
}
