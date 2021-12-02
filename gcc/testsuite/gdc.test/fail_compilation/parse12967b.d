/*
TEST_OUTPUT:
---
fail_compilation/parse12967b.d(24): Error: function `parse12967b.C.pre_c` without `this` cannot be `const`
fail_compilation/parse12967b.d(25): Error: function `parse12967b.C.pre_i` without `this` cannot be `immutable`
fail_compilation/parse12967b.d(26): Error: function `parse12967b.C.pre_w` without `this` cannot be `inout`
fail_compilation/parse12967b.d(27): Error: function `parse12967b.C.pre_s` without `this` cannot be `shared`
fail_compilation/parse12967b.d(29): Error: function `parse12967b.C.post_c` without `this` cannot be `const`
fail_compilation/parse12967b.d(30): Error: function `parse12967b.C.post_i` without `this` cannot be `immutable`
fail_compilation/parse12967b.d(31): Error: function `parse12967b.C.post_w` without `this` cannot be `inout`
fail_compilation/parse12967b.d(32): Error: function `parse12967b.C.post_s` without `this` cannot be `shared`
fail_compilation/parse12967b.d(37): Error: function `parse12967b.D.pre_c` without `this` cannot be `const`
fail_compilation/parse12967b.d(38): Error: function `parse12967b.D.pre_i` without `this` cannot be `immutable`
fail_compilation/parse12967b.d(39): Error: function `parse12967b.D.pre_w` without `this` cannot be `inout`
fail_compilation/parse12967b.d(40): Error: function `parse12967b.D.pre_s` without `this` cannot be `shared`
fail_compilation/parse12967b.d(41): Error: function `parse12967b.D.post_c` without `this` cannot be `const`
fail_compilation/parse12967b.d(42): Error: function `parse12967b.D.post_i` without `this` cannot be `immutable`
fail_compilation/parse12967b.d(43): Error: function `parse12967b.D.post_w` without `this` cannot be `inout`
fail_compilation/parse12967b.d(44): Error: function `parse12967b.D.post_s` without `this` cannot be `shared`
---
*/
class C
{
    const     static      pre_c() {}
    immutable static      pre_i() {}
    inout     static      pre_w() {}
    shared    static      pre_s() {}

    static      post_c() const     {}
    static      post_i() immutable {}
    static      post_w() inout     {}
    static      post_s() shared    {}
}

class D
{
    const     static void pre_c() {}
    immutable static void pre_i() {}
    inout     static void pre_w() {}
    shared    static void pre_s() {}
    static void post_c() const     {}
    static void post_i() immutable {}
    static void post_w() inout     {}
    static void post_s() shared    {}
}
