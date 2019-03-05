// REQUIRED_ARGS: -de
import imports.dip22;

class Foo : Base1, Base2
{
    void test()
    {
        static assert(typeof(bar()).sizeof == 2);
        static assert(baz == 2);
        static assert(T.sizeof == 2);
    }
}

void test()
{
    bar(12);
    baz(12);
    12.bar();
    12.baz();
}
