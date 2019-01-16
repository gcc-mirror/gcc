struct S7424g
{
    @property int g()() immutable { return 0; }
    void test() inout { int f = g; }
}

