// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/fail11720.d(23): Error: declaration fail11720.foo!().foo.temp is already defined in another scope in foo
fail_compilation/fail11720.d(13): Error: template instance fail11720.foo!() error instantiating
fail_compilation/fail11720.d(31): Error: declaration fail11720.bar.temp is already defined in another scope in bar
---
*/

void main()
{
    foo();
    bar();
}

alias TypeTuple(T...) = T;

void foo()()
{
    foreach (T; TypeTuple!(int, double))
    {
        static temp = T.stringof;
    }
}

void bar()
{
    foreach (T; TypeTuple!(int, double))
    {
        static temp = T.stringof;
    }
}
