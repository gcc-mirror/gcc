struct S7424g
{
    @property int g()() { return 0; }
    void test() shared { int f = g; }
}

