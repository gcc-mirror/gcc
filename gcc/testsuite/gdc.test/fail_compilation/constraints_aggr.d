/*
EXTRA_FILES: imports/constraints.d
TEST_OUTPUT:
---
fail_compilation/constraints_aggr.d(32): Error: none of the overloads of template `imports.constraints.C.f` are callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(60):        Candidate is: `f(T)(T v)`
  with `T = int`
  must satisfy the following constraint:
`       !P!T`
fail_compilation/constraints_aggr.d(33): Error: none of the overloads of template `imports.constraints.C.g` are callable using argument types `!()()`
fail_compilation/imports/constraints.d(63):        Candidate is: `g(this T)()`
  with `T = imports.constraints.C`
  must satisfy the following constraint:
`       N!T`
fail_compilation/constraints_aggr.d(35): Error: template instance `imports.constraints.S!int` does not match template declaration `S(T)`
  with `T = int`
  must satisfy the following constraint:
`       N!T`
fail_compilation/constraints_aggr.d(44): Error: template instance `imports.constraints.BitFlags!(Enum)` does not match template declaration `BitFlags(E, bool unsafe = false)`
  with `E = Enum`
  must satisfy one of the following constraints:
`       unsafe
       N!E`
---
*/

void main()
{
    import imports.constraints;

    C c = new C;
    c.f(0);
    c.g();

    S!int;

    enum Enum
    {
        A = 1,
        B = 2,
        C = 4,
        BC = B|C
    }
    BitFlags!Enum flags;
}
