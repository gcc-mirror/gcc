/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test16193.d(39): Error: function `test16193.abc` is `@nogc` yet allocates closure for `abc()` with the GC
fail_compilation/test16193.d(41):        delegate `int(int i) => 0` closes over variable `x`
fail_compilation/test16193.d(40):        `x` declared here
---
*/
//fail_compilation/test16193.d(22): To enforce `@safe`, the compiler allocates a closure unless `opApply()` uses `scope`
//fail_compilation/test16193.d(34): To enforce `@safe`, the compiler allocates a closure unless `opApply()` uses `scope`
//fail_compilation/test16193.d(41): To enforce `@safe`, the compiler allocates a closure unless `opApply()` uses `scope`

// https://issues.dlang.org/show_bug.cgi?id=16193

struct S {
    int opApply(int delegate(int) dg) @nogc;
}

void foo() {
    int x = 0;
    foreach(i; S.init) {
        x++;
    }
}

struct T {
    int opApply(scope int delegate(int) dg) @nogc;
}


void bar() @nogc {
    int x = 0;
    foreach(i; T.init) {
        x++;
    }
}

void abc() @nogc {
    int x = 0;
    foreach(i; S.init) {
        x++;
    }
}
