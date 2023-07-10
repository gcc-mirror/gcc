/*
TEST_OUTPUT:
---
fail_compilation/diag8648.d(18): Error: undefined identifier `X`
fail_compilation/diag8648.d(29): Error: template `diag8648.foo` is not callable using argument types `!()(Foo!(int, 1))`
fail_compilation/diag8648.d(18):        Candidate is: `foo(T, n)(X!(T, n))`
fail_compilation/diag8648.d(20): Error: undefined identifier `a`
fail_compilation/diag8648.d(31): Error: template `diag8648.bar` is not callable using argument types `!()(Foo!(int, 1))`
fail_compilation/diag8648.d(20):        Candidate is: `bar(T)(Foo!(T, a))`
fail_compilation/diag8648.d(20): Error: undefined identifier `a`
fail_compilation/diag8648.d(32): Error: template `diag8648.bar` is not callable using argument types `!()(Foo!(int, f))`
fail_compilation/diag8648.d(20):        Candidate is: `bar(T)(Foo!(T, a))`
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
