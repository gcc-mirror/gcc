/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fix18575.d(27): Error: returning `s.foo()` escapes a reference to parameter `s`
fail_compilation/fix18575.d(31): Error: returning `s.foo()` escapes a reference to parameter `s`
fail_compilation/fix18575.d(35): Error: returning `s.abc()` escapes a reference to parameter `s`
fail_compilation/fix18575.d(39): Error: returning `s.ghi(t)` escapes a reference to parameter `t`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=18575

@safe:

struct S {
@safe:
    int x;

    void bar() { }
    auto foo() { return &this.bar; }
    auto def() { return &bar; }
    auto abc() { return &x; }
    auto ghi(ref S s) { return &s.bar; }
}

auto f(S s) {
    return s.foo();
}

auto g(S s) {
    return s.foo();
}

auto h(S s) {
    return s.abc();
}

auto j(S s, S t) {
    return s.ghi(t);
}
