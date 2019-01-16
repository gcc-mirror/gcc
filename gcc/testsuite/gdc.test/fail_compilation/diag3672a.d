// PERMUTE_ARGS:
// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/diag3672a.d(16): Deprecation: read-modify-write operations are not allowed for shared variables. Use core.atomic.atomicOp!"+="(ns.x, 1) instead.
fail_compilation/diag3672a.d(18): Deprecation: read-modify-write operations are not allowed for shared variables. Use core.atomic.atomicOp!"+="(s.sx, 1) instead.
---
*/
class NS { shared int x; }
shared class S { int sx; }

void main()
{
    NS ns = new NS;
    ns.x++;
    S s = new S;
    s.sx++;
}

/*
TEST_OUTPUT:
---
fail_compilation/diag3672a.d(32): Deprecation: read-modify-write operations are not allowed for shared variables. Use core.atomic.atomicOp!"+="(s.var, 1) instead.
fail_compilation/diag3672a.d(33): Deprecation: read-modify-write operations are not allowed for shared variables. Use core.atomic.atomicOp!"-="(s.var, 2) instead.
---
*/
void test13003()
{
    struct S { int var; }
    shared S s;
    s.var++;
    s.var -= 2;
}
