/*
REQUIRED_ARGS: -Jdoes_not_exists -Jfail_compilation/fail1995.d -Jfail_compilation/
TEST_OUTPUT:
---
fail_compilation/fail1995.d(12): Error: file `"SomeFile.txt"` cannot be found or not in a path specified with `-J`
fail_compilation/fail1995.d(12):        Path(s) searched (as provided by `-J`):
fail_compilation/fail1995.d(12):        [0]: `does_not_exists` (path not found)
fail_compilation/fail1995.d(12):        [1]: `fail_compilation/fail1995.d` (not a directory)
fail_compilation/fail1995.d(12):        [2]: `fail_compilation/`
---
 */
immutable string Var = import("SomeFile.txt");
