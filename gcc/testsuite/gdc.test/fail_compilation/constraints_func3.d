/*
EXTRA_FILES: imports/constraints.d
TEST_OUTPUT:
---
fail_compilation/constraints_func3.d(53): Error: none of the overloads of template `imports.constraints.overload` are callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(39):        Candidates are: `overload(T)(T v)`
  with `T = int`
  must satisfy the following constraint:
`       N!T`
fail_compilation/imports/constraints.d(40):                        `overload(T)(T v)`
  with `T = int`
  must satisfy the following constraint:
`       !P!T`
fail_compilation/imports/constraints.d(41):                        `overload(T)(T v1, T v2)`
fail_compilation/imports/constraints.d(42):                        `overload(T, V)(T v1, V v2)`
fail_compilation/constraints_func3.d(54): Error: none of the overloads of template `imports.constraints.overload` are callable using argument types `!()(int, string)`
fail_compilation/imports/constraints.d(39):        Candidates are: `overload(T)(T v)`
fail_compilation/imports/constraints.d(40):                        `overload(T)(T v)`
fail_compilation/imports/constraints.d(41):                        `overload(T)(T v1, T v2)`
fail_compilation/imports/constraints.d(42):                        `overload(T, V)(T v1, V v2)`
  with `T = int,
       V = string`
  must satisfy one of the following constraints:
`       N!T
       N!V`
fail_compilation/constraints_func3.d(56): Error: template `imports.constraints.variadic` is not callable using argument types `!()()`
fail_compilation/imports/constraints.d(43):        Candidate is: `variadic(A, T...)(A a, T v)`
fail_compilation/constraints_func3.d(57): Error: template `imports.constraints.variadic` is not callable using argument types `!()(int)`
fail_compilation/imports/constraints.d(43):        Candidate is: `variadic(A, T...)(A a, T v)`
  with `A = int,
       T = ()`
  must satisfy the following constraint:
`       N!int`
fail_compilation/constraints_func3.d(58): Error: template `imports.constraints.variadic` is not callable using argument types `!()(int, int)`
fail_compilation/imports/constraints.d(43):        Candidate is: `variadic(A, T...)(A a, T v)`
  with `A = int,
       T = (int)`
  must satisfy the following constraint:
`       N!int`
fail_compilation/constraints_func3.d(59): Error: template `imports.constraints.variadic` is not callable using argument types `!()(int, int, int)`
fail_compilation/imports/constraints.d(43):        Candidate is: `variadic(A, T...)(A a, T v)`
  with `A = int,
       T = (int, int)`
  must satisfy the following constraint:
`       N!int`
---
*/

void main()
{
    import imports.constraints;

    overload(0);
    overload(0, "");

    variadic();
    variadic(0);
    variadic(0, 1);
    variadic(0, 1, 2);
}
