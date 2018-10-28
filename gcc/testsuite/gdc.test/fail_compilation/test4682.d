/*
TEST_OUTPUT:
----
fail_compilation/test4682.d(10): Error: integer overflow: `int.min / -1`
fail_compilation/test4682.d(11): Error: integer overflow: `long.min / -1L`
fail_compilation/test4682.d(12): Error: integer overflow: `int.min % -1`
fail_compilation/test4682.d(13): Error: integer overflow: `long.min % -1L`
----
*/
auto a = int.min / -1;
auto b = long.min / -1;
auto c = int.min % -1;
auto d = long.min % -1;
