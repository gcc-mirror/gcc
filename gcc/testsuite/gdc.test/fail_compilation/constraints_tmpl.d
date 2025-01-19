/*
EXTRA_FILES: imports/constraints.d
TEST_OUTPUT:
---
fail_compilation/constraints_tmpl.d(35): Error: template instance `imports.constraints.dummy!()` does not match template declaration `dummy()()`
  must satisfy the following constraint:
`       false`
fail_compilation/constraints_tmpl.d(37): Error: template instance `imports.constraints.message_nice!(int, int)` does not match template declaration `message_nice(T, U)()`
  with `T = int,
       U = int`
  must satisfy the following constraint:
`       N!U`
fail_compilation/constraints_tmpl.d(38): Error: template instance `imports.constraints.message_ugly!int` does not match template declaration `message_ugly(T)(T v)`
  with `T = int`
  must satisfy the following constraint:
`       N!T`
fail_compilation/constraints_tmpl.d(40): Error: template instance `args!int` does not match template declaration `args(T, U)()`
fail_compilation/constraints_tmpl.d(41): Error: template instance `imports.constraints.args!(int, float)` does not match template declaration `args(T, U)()`
  with `T = int,
       U = float`
  must satisfy one of the following constraints:
`       N!T
       N!U`
fail_compilation/constraints_tmpl.d(43): Error: template instance `constraints_tmpl.main.lambda!((a) => a)` does not match template declaration `lambda(alias pred)()`
  with `pred = __lambda_L43_C13`
  must satisfy the following constraint:
`       N!int`
---
*/

void main()
{
    import imports.constraints;

    dummy!();

    message_nice!(int, int);
    message_ugly!int;

    args!int;
    args!(int, float);

    lambda!(a => a)();
}
