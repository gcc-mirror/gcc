// https://issues.dlang.org/show_bug.cgi?id=243645

/*
TEST_OUTPUT:
---
fail_compilation/test24365.d(16): Error: `f` cannot be interpreted at compile time, because it has no available source code
fail_compilation/test24365.d(14):        compile time context created here
fail_compilation/test24365.d(19):        while evaluating: `static assert(r == 2)`
---
*/

void main()
{
    enum r = () {
        void f();
        f();
        return 2;
    }();
    static assert(r == 2);
}
