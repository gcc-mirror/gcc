/*
TEST_OUTPUT:
---
fail_compilation/b20011.d(28): Error: cannot modify expression `S1(cast(ubyte)0u).member` because it is not an lvalue
fail_compilation/b20011.d(31): Error: cannot modify expression `S2(null).member` because it is not an lvalue
fail_compilation/b20011.d(32): Error: cannot modify expression `S2(null).member` because it is not an lvalue
fail_compilation/b20011.d(35): Error: cannot modify expression `U1(cast(ubyte)0u, ).m2` because it is not an lvalue
fail_compilation/b20011.d(40): Error: function `assignableByRef` is not callable using argument types `(ubyte)`
fail_compilation/b20011.d(40):        cannot pass rvalue argument `S1(cast(ubyte)0u).member` of type `ubyte` to parameter `ref ubyte p`
fail_compilation/b20011.d(37):        `b20011.main.assignableByRef(ref ubyte p)` declared here
fail_compilation/b20011.d(41): Error: function `assignableByOut` is not callable using argument types `(ubyte)`
fail_compilation/b20011.d(41):        cannot pass rvalue argument `S1(cast(ubyte)0u).member` of type `ubyte` to parameter `out ubyte p`
fail_compilation/b20011.d(38):        `b20011.main.assignableByOut(out ubyte p)` declared here
fail_compilation/b20011.d(42): Error: function `assignableByConstRef` is not callable using argument types `(ubyte)`
fail_compilation/b20011.d(42):        cannot pass rvalue argument `S1(cast(ubyte)0u).member` of type `ubyte` to parameter `ref const(ubyte) p`
fail_compilation/b20011.d(39):        `b20011.main.assignableByConstRef(ref const(ubyte) p)` declared here
---
*/
module b20011;

struct S1 { ubyte member;     }
struct S2 { ubyte[] member;   }
union U1  { ubyte m1; int m2; }

void main()
{
    enum S1 s1 = {};
    s1.member = 42;

    enum S2 s2 = {};
    s2.member = [];
    s2.member ~= [];

    enum U1 u1 = {m1 : 0};
    u1.m2 = 42;

    void assignableByRef(ref ubyte p){ p = 42; }
    void assignableByOut(out ubyte p){ p = 42; }
    void assignableByConstRef(ref const ubyte p){}
    assignableByRef(s1.member);
    assignableByOut(s1.member);
    assignableByConstRef(s1.member);
}
