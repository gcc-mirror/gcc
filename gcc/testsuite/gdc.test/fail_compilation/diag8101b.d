/*
TEST_OUTPUT:
---
fail_compilation/diag8101b.d(27): Error: none of the overloads of `foo` are callable using argument types `(double)`, candidates are:
fail_compilation/diag8101b.d(18):        `diag8101b.S.foo(int _param_0)`
fail_compilation/diag8101b.d(19):        `diag8101b.S.foo(int _param_0, int _param_1)`
fail_compilation/diag8101b.d(29): Error: function `diag8101b.S.bar(int _param_0)` is not callable using argument types `(double)`
fail_compilation/diag8101b.d(29):        cannot pass argument `1.00000` of type `double` to parameter `int _param_0`
fail_compilation/diag8101b.d(32): Error: none of the overloads of `foo` are callable using a `const` object, candidates are:
fail_compilation/diag8101b.d(18):        `diag8101b.S.foo(int _param_0)`
fail_compilation/diag8101b.d(19):        `diag8101b.S.foo(int _param_0, int _param_1)`
fail_compilation/diag8101b.d(34): Error: mutable method `diag8101b.S.bar` is not callable using a `const` object
---
*/

struct S
{
    void foo(int) { }
    void foo(int, int) { }

    void bar(int) { }
}

void main()
{
    S s;
    s.foo(1.0);

    s.bar(1.0);

    const(S) cs;
    cs.foo(1);

    cs.bar(1);
}
