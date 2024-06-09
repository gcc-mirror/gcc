/*
TEST_OUTPUT:
---
fail_compilation/diag8101b.d(28): Error: none of the overloads of `foo` are callable using argument types `(double)`
fail_compilation/diag8101b.d(19):        Candidates are: `diag8101b.S.foo(int __param_0)`
fail_compilation/diag8101b.d(20):                        `diag8101b.S.foo(int __param_0, int __param_1)`
fail_compilation/diag8101b.d(30): Error: function `diag8101b.S.bar(int __param_0)` is not callable using argument types `(double)`
fail_compilation/diag8101b.d(30):        cannot pass argument `1.0` of type `double` to parameter `int __param_0`
fail_compilation/diag8101b.d(33): Error: none of the overloads of `foo` are callable using a `const` object with argument types `(int)`
fail_compilation/diag8101b.d(19):        Candidates are: `diag8101b.S.foo(int __param_0)`
fail_compilation/diag8101b.d(20):                        `diag8101b.S.foo(int __param_0, int __param_1)`
fail_compilation/diag8101b.d(35): Error: mutable method `diag8101b.S.bar` is not callable using a `const` object
fail_compilation/diag8101b.d(22):        Consider adding `const` or `inout` here
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
