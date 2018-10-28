struct S7424f
{
    @property int g()() shared { return 0; }
    void test() { int f = g; }
}

