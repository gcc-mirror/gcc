/*
TEST_OUTPUT:
---
fail_compilation/fail10115.d(35): Error: cannot have `out` parameter of type `S` because the default construction is disabled
fail_compilation/fail10115.d(35): Error: cannot have `out` parameter of type `E` because the default construction is disabled
fail_compilation/fail10115.d(35): Error: cannot have `out` parameter of type `U` because the default construction is disabled
fail_compilation/fail10115.d(40): Error: struct `fail10115.S` default construction is disabled
fail_compilation/fail10115.d(41): Error: struct `fail10115.S` default construction is disabled
fail_compilation/fail10115.d(42): Error: union `fail10115.U` default construction is disabled
---
*/

struct S
{
    int a;
    @disable this();
    //this(int) { a = 1; }
    //~this() { assert(a !is 0); }
}

enum E : S
{
    A = S.init
}

union U
{
    S s;
    //this(this) { assert (s.a !is 0); }
    //~this() { assert (s.a !is 0); }
}

void main()
{
    void foo(out S s, out E e, out U u) { }

    S[] a;
    E[] e;
    U[] u;
    a.length = 5;   // compiles -> NG
    e.length = 5;   // compiles -> NG
    u.length = 5;   // compiles -> NG

    S[1] x = (S[1]).init;
    foo(a[0],       // compiles -> NG
        e[0],       // compiles -> NG
        u[0]);      // compiles -> NG
}
