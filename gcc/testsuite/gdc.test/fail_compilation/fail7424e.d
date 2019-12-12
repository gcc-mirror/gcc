struct S7424e
{
    @property int g()() immutable { return 0; }
    void test() { int f = g; }
}

