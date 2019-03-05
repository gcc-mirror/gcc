/*
REQUIRED_ARGS: -dip1000 -dip25
TEST_OUTPUT:
---
fail_compilation/test17450.d(15): Error: returning `&s.bar` escapes a reference to parameter `s`, perhaps annotate with `return`
fail_compilation/test17450.d(18): Error: returning `&this.bar` escapes a reference to parameter `this`, perhaps annotate with `return`
---
*/
// https://issues.dlang.org/show_bug.cgi?id=17450

alias dg_t = void delegate();

struct S {
    @safe dg_t foo1(ref S s) {
        return &s.bar;
    }
    @safe dg_t foo2() {
        return &bar;
    }

    @safe dg_t foo3(return ref S s) {
        return &s.bar;
    }
    @safe dg_t foo4() return {
        return &bar;
    }

    @safe void bar();
}

/*
TEST_OUTPUT:
---
fail_compilation/test17450.d(103): Error: scope variable `c` may not be returned
fail_compilation/test17450.d(106): Error: scope variable `this` may not be returned
---
*/
// https://issues.dlang.org/show_bug.cgi?id=17450

#line 100

class C {
    @safe dg_t foo1(scope C c) {
        return &c.bar;
    }
    @safe dg_t foo2() scope {
        return &bar;
    }

    @safe dg_t foo3(return scope C c) {
        return &c.bar;
    }
    @safe dg_t foo4() return scope {
        return &bar;
    }

    @safe void bar();
}
