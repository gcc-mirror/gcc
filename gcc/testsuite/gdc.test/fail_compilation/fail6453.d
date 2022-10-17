/*
TEST_OUTPUT:
---
fail_compilation/fail6453.d(13): Error: struct `fail6453.S6453x` mixing invariants with different `shared`/`synchronized` qualifiers is not supported
fail_compilation/fail6453.d(18): Error: class `fail6453.C6453y` mixing invariants with different `shared`/`synchronized` qualifiers is not supported
fail_compilation/fail6453.d(23): Error: class `fail6453.C6453z` mixing invariants with different `shared`/`synchronized` qualifiers is not supported
---
*/

struct S6453x
{
           invariant() {}
    shared invariant() {}
}
class C6453y
{
           invariant() {}
    synchronized invariant() {}
}
class C6453z
{
          shared invariant() {}
    synchronized invariant() {}
}
