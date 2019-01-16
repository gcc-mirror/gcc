// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

struct X
{
    void mfoo(this T)() {}
}
void test()
{
    shared const X scx;

    scx.mfoo();
}

struct Vec
{
    int x;
    void sc() shared const {}
}
