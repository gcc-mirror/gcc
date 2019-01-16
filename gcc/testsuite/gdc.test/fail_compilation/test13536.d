/*
PERMUTE_ARGS:
TEST_OUTPUT:
---
fail_compilation/test13536.d(24): Error: field U.sysDg cannot access pointers in @safe code that overlap other fields
fail_compilation/test13536.d(24): Error: address of variable `s` assigned to `u` with longer lifetime
fail_compilation/test13536.d(25): Error: field U.safeDg cannot access pointers in @safe code that overlap other fields
---
*/


// https://issues.dlang.org/show_bug.cgi?id=13536

struct S {
    void sysMethod() @system {}
}
void fun() @safe {
    union U {
        void delegate() @system sysDg;
        void delegate() @safe safeDg;
    }
    U u;
    S s;
    u.sysDg = &s.sysMethod;
    u.safeDg();
}

