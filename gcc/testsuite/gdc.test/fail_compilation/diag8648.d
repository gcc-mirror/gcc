/*
TEST_OUTPUT:
---
fail_compilation/diag8648.d(18): Error: undefined identifier `X`
fail_compilation/diag8648.d(29): Error: template diag8648.foo cannot deduce function from argument types !()(Foo!(int, 1)), candidates are:
fail_compilation/diag8648.d(18):        diag8648.foo(T, n)(X!(T, n))
fail_compilation/diag8648.d(20): Error: undefined identifier `a`
fail_compilation/diag8648.d(31): Error: template diag8648.bar cannot deduce function from argument types !()(Foo!(int, 1)), candidates are:
fail_compilation/diag8648.d(20):        diag8648.bar(T)(Foo!(T, a))
fail_compilation/diag8648.d(20): Error: undefined identifier `a`
fail_compilation/diag8648.d(32): Error: template diag8648.bar cannot deduce function from argument types !()(Foo!(int, f)), candidates are:
fail_compilation/diag8648.d(20):        diag8648.bar(T)(Foo!(T, a))
---
*/

struct Foo(T, alias a) {}

void foo(T, n)(X!(T, n) ) {}    // undefined identifier 'X'

void bar(T)(Foo!(T, a) ) {}     // undefined identifier 'a'

void main()
{
    template f() {}

    Foo!(int, 1) x;
    Foo!(int, f) y;

    foo(x);

    bar(x); // expression '1' vs undefined Type 'a'
    bar(y); // symbol 'f' vs undefined Type 'a'
}
