/*
TEST_OUTPUT:
---
fail_compilation/fail4421.d(16): Error: function `fail4421.U1.__postblit` destructors, postblits and invariants are not allowed in union `U1`
fail_compilation/fail4421.d(17): Error: destructor `fail4421.U1.~this` destructors, postblits and invariants are not allowed in union `U1`
fail_compilation/fail4421.d(18): Error: function `fail4421.U1.__invariant1` destructors, postblits and invariants are not allowed in union `U1`
---




*/

union U1
{
    this(this);
    ~this();
    invariant() { }
}

struct S1
{
    this(this);
    ~this();
    invariant() { }
}

union U2
{
    S1 s1;
}

struct S2
{
    union
    {
        S1 s1;
        int j;
    }
}
