// https://issues.dlang.org/show_bug.cgi?id=22626
// REQUIRED_ARGS: -preview=nosharedaccess

shared int k;

class Oops
{
    shared int a;
    shared int* pa;
    synchronized void oops()
    {
        // this should compile since the function is synchronized
        // and `a` is accessed through `this`.
        a = 2;

        // this shouldn't compile because synchronized guards
        // only accesses to the first level of dereferencing
        static assert (!__traits(compiles, *pa = 2));

        // this shouldn't compile `k` is a field of class `Oops`
        static assert (!__traits(compiles, k = 2));
    }

    // https://issues.dlang.org/show_bug.cgi?id=24705
    synchronized void foo(int n)
    {
        // Error: direct access to shared `n` is not allowed, see `core.atomic`
        int a = n;
    }
}
