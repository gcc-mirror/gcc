struct S7424d
{
    @property int g()() immutable { return 0; }
    void test() const { int f = g; }
}

