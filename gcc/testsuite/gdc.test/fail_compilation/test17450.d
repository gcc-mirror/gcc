/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test17450.d(17): Error: escaping a reference to parameter `s` by returning `&s.bar` is not allowed in a `@safe` function
fail_compilation/test17450.d(16):        perhaps annotate the parameter with `return`
fail_compilation/test17450.d(20): Error: escaping a reference to parameter `this` by returning `&this.bar` is not allowed in a `@safe` function
fail_compilation/test17450.d(19):        perhaps annotate the function with `return`
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
fail_compilation/test17450.d(103): Error: scope parameter `c` may not be returned
fail_compilation/test17450.d(106): Error: scope parameter `this` may not be returned
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
