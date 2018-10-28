/*
TEST_OUTPUT:
---
fail_compilation/diag8928.d(18): Error: class diag8928.Z cannot implicitly generate a default ctor when base class diag8928.X is missing a default ctor
---
*/

class X
{
    this(int n) {}
}

class Y : X
{
    this() {}
}

class Z : X
{
}
