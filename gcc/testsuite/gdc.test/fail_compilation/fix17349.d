/* TEST_OUTPUT:
---
fail_compilation/fix17349.d(35): Error: cannot implicitly override base class method `fix17349.E.foo` with `fix17349.F.foo`; add `override` attribute
---
 */

// https://issues.dlang.org/show_bug.cgi?id=17349

struct S { }

class C {
    void bar();
    void foo(void* p);
    void abc(Object);
    void def(S);
}

class D : C {
    override void bar() const;
    override void foo(const void*);
    override void abc(const Object);
    override void def(const S);
}

alias fp_t = void function(int*);
@safe void abc(const int*);
fp_t fp = &abc;


class E {
    void foo(void*);
}

class F : E {
    void foo(const void*);
}
