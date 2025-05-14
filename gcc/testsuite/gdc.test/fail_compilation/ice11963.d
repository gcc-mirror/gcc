/*
TEST_OUTPUT:
---
fail_compilation/ice11963.d(10): Error: unexpected `(` in declarator
fail_compilation/ice11963.d(10): Error: identifier expected for template type parameter
fail_compilation/ice11963.d(10): Error: variable name expected after type `A`, not `""`
fail_compilation/ice11963.d(10): Error: declaration expected, not `""`
---
*/
A("")=
