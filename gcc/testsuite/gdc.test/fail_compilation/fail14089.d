/*
TEST_OUTPUT:
---
fail_compilation/fail14089.d(41): Error: `1` has no effect
fail_compilation/fail14089.d(41): Error: `1` has no effect
fail_compilation/fail14089.d(42): Error: `1` has no effect
fail_compilation/fail14089.d(42): Error: `n` has no effect
fail_compilation/fail14089.d(43): Error: `1` has no effect
fail_compilation/fail14089.d(43): Error: `s.val` has no effect
fail_compilation/fail14089.d(44): Error: `n` has no effect
fail_compilation/fail14089.d(44): Error: `1` has no effect
fail_compilation/fail14089.d(45): Error: `s.val` has no effect
fail_compilation/fail14089.d(45): Error: `1` has no effect
---
*/

bool cond;

void main()
{
    int foo() { return 0; }
    int n;
    struct S { int val; }
    S s;

    // The whole of each CondExps has side effects, So no error.
    cond ? foo() : n;
    cond ? foo() : s.val;
    cond ? 1     : foo();
    cond ? n     : foo();
    cond ? s.val : foo();

    cond ? (n = 1) : 1;
    cond ? (n = 1) : n;
    cond ? (n = 1) : s.val;
    cond ? 1       : (n = 1);
    cond ? n       : (n = 1);
    cond ? s.val   : (n = 1);

    // errors
    cond ? 1     : 1;
    cond ? 1     : n;
    cond ? 1     : s.val;
    cond ? n     : 1;
    cond ? s.val : 1;
}
