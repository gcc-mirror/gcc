/*
EXTRA_FILES: imports/constraints.d
REQUIRED_ARGS: -verrors=context
TEST_OUTPUT:
---
fail_compilation/constraints_func4.d(90): Error: none of the overloads of template `imports.constraints.overload` are callable using argument types `!()(int)`
    overload(0);
            ^
fail_compilation/imports/constraints.d(39):        Candidates are: `overload(T)(T v)`
  with `T = int`
  must satisfy the following constraint:
`       N!T`
void overload(T)(T v) if (N!T);
     ^
fail_compilation/imports/constraints.d(40):                        `overload(T)(T v)`
  with `T = int`
  must satisfy the following constraint:
`       !P!T`
void overload(T)(T v) if (!P!T);
     ^
fail_compilation/imports/constraints.d(41):                        `overload(T)(T v1, T v2)`
void overload(T)(T v1, T v2) if (N!T);
     ^
fail_compilation/imports/constraints.d(42):                        `overload(T, V)(T v1, V v2)`
void overload(T, V)(T v1, V v2) if (N!T || N!V);
     ^
fail_compilation/constraints_func4.d(91): Error: none of the overloads of template `imports.constraints.overload` are callable using argument types `!()(int, string)`
    overload(0, "");
            ^
fail_compilation/imports/constraints.d(39):        Candidates are: `overload(T)(T v)`
void overload(T)(T v) if (N!T);
     ^
fail_compilation/imports/constraints.d(40):                        `overload(T)(T v)`
void overload(T)(T v) if (!P!T);
     ^
fail_compilation/imports/constraints.d(41):                        `overload(T)(T v1, T v2)`
void overload(T)(T v1, T v2) if (N!T);
     ^
fail_compilation/imports/constraints.d(42):                        `overload(T, V)(T v1, V v2)`
  with `T = int,
       V = string`
  must satisfy one of the following constraints:
`       N!T
       N!V`
void overload(T, V)(T v1, V v2) if (N!T || N!V);
     ^
fail_compilation/constraints_func4.d(93): Error: none of the overloads of template `imports.constraints.variadic` are callable using argument types `!()()`
    variadic();
            ^
fail_compilation/imports/constraints.d(43):        Candidate is: `variadic(A, T...)(A a, T v)`
void variadic(A, T...)(A a, T v) if (N!int);
     ^
fail_compilation/constraints_func4.d(94): Error: none of the overloads of template `imports.constraints.variadic` are callable using argument types `!()(int)`
    variadic(0);
            ^
fail_compilation/imports/constraints.d(43):        Candidate is: `variadic(A, T...)(A a, T v)`
  with `A = int,
       T = ()`
  must satisfy the following constraint:
`       N!int`
void variadic(A, T...)(A a, T v) if (N!int);
     ^
fail_compilation/constraints_func4.d(95): Error: none of the overloads of template `imports.constraints.variadic` are callable using argument types `!()(int, int)`
    variadic(0, 1);
            ^
fail_compilation/imports/constraints.d(43):        Candidate is: `variadic(A, T...)(A a, T v)`
  with `A = int,
       T = (int)`
  must satisfy the following constraint:
`       N!int`
void variadic(A, T...)(A a, T v) if (N!int);
     ^
fail_compilation/constraints_func4.d(96): Error: none of the overloads of template `imports.constraints.variadic` are callable using argument types `!()(int, int, int)`
    variadic(0, 1, 2);
            ^
fail_compilation/imports/constraints.d(43):        Candidate is: `variadic(A, T...)(A a, T v)`
  with `A = int,
       T = (int, int)`
  must satisfy the following constraint:
`       N!int`
void variadic(A, T...)(A a, T v) if (N!int);
     ^
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
