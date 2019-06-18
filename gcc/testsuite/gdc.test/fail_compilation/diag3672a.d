// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/diag3672a.d(17): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672a.d(17):        Use `core.atomic.atomicOp!"+="(ns.x, 1)` instead
fail_compilation/diag3672a.d(19): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672a.d(19):        Use `core.atomic.atomicOp!"+="(s.sx, 1)` instead
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
fail_compilation/diag3672a.d(35): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672a.d(35):        Use `core.atomic.atomicOp!"+="(s.var, 1)` instead
fail_compilation/diag3672a.d(36): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672a.d(36):        Use `core.atomic.atomicOp!"-="(s.var, 2)` instead
---
*/
void test13003()
{
    struct S { int var; }
    shared S s;
    s.var++;
    s.var -= 2;
}
