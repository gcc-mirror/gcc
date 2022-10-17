/*
TEST_OUTPUT:
---
fail_compilation/diag5450.d(18): Error: class `diag5450.C` cannot implicitly generate a default constructor when base class `diag5450.B` is missing a default constructor
---
*/

class A
{
    this() { }
}

class B : A
{
    this(int f) {}
}

class C : B
{
}
