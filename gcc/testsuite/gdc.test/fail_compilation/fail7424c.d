struct S7424c
{
    @property int g()() { return 0; }
    void test() immutable { int f = g; }
}

