struct S7424b
{
    @property int g()() { return 0; }
    void test() const { int f = g; }
}
