/*
EXTRA_FILES: imports/constraints.d
TEST_OUTPUT:
---
fail_compilation/constraints_defs.d(49): Error: template instance `constraints_defs.main.def!(int, 0, (a) => a)` does not match template declaration `def(T, int i = 5, alias R)()`
  with `T = int,
       i = 0,
       R = __lambda_L49_C18`
  must satisfy the following constraint:
`       N!T`
fail_compilation/constraints_defs.d(50): Error: template instance `imports.constraints.defa!int` does not match template declaration `defa(T, U = int)()`
  with `T = int`
  must satisfy the following constraint:
`       N!T`
fail_compilation/constraints_defs.d(51): Error: template instance `imports.constraints.defv!()` does not match template declaration `defv(T = bool, int i = 5, Ts...)()`
  with `Ts = ()`
  must satisfy the following constraint:
`       N!T`
fail_compilation/constraints_defs.d(52): Error: template instance `imports.constraints.defv!int` does not match template declaration `defv(T = bool, int i = 5, Ts...)()`
  with `T = int,
       Ts = ()`
  must satisfy the following constraint:
`       N!T`
fail_compilation/constraints_defs.d(53): Error: template instance `imports.constraints.defv!(int, 0)` does not match template declaration `defv(T = bool, int i = 5, Ts...)()`
  with `T = int,
       i = 0,
       Ts = ()`
  must satisfy the following constraint:
`       N!T`
fail_compilation/constraints_defs.d(54): Error: template instance `imports.constraints.defv!(int, 0, bool)` does not match template declaration `defv(T = bool, int i = 5, Ts...)()`
  with `T = int,
       i = 0,
       Ts = (bool)`
  must satisfy the following constraint:
`       N!T`
fail_compilation/constraints_defs.d(55): Error: template instance `imports.constraints.defv!(int, 0, bool, float)` does not match template declaration `defv(T = bool, int i = 5, Ts...)()`
  with `T = int,
       i = 0,
       Ts = (bool, float)`
  must satisfy the following constraint:
`       N!T`
---
*/

void main()
{
    import imports.constraints;

    def!(int, 0, a => a)();
    defa!(int)();
    defv!()();
    defv!(int)();
    defv!(int, 0)();
    defv!(int, 0, bool)();
    defv!(int, 0, bool, float)();
}
