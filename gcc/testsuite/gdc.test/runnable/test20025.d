// https://issues.dlang.org/show_bug.cgi?id=20025
struct B
{
        static int value = 77;
        alias value this;

        this(ref return scope inout B rhs) inout { }
}

void test(int x)
{
        assert(x == 77);
}

int main()
{
        B b;
        test(b);

        return 0;
}
