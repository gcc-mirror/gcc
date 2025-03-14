/*
TEST_OUTPUT:
---
fail_compilation/traits_alone.d(11): Error: found `End of File` when expecting `(`
fail_compilation/traits_alone.d(11): Error: `__traits(identifier, args...)` expected
fail_compilation/traits_alone.d(11): Error: variable name expected after type `$r:_?_error_?$`, not `End of File`
---
*/
//used to segfault
__traits
