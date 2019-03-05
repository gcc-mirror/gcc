/*
TEST_OUTPUT:
----
fail_compilation/test4682a.d(10): Error: divide by 0
fail_compilation/test4682a.d(11): Error: divide by 0
fail_compilation/test4682a.d(12): Error: divide by 0
fail_compilation/test4682a.d(13): Error: divide by 0
----
*/
auto a = int.min / 0;
auto b = long.min / 0;
auto c = int.min % 0;
auto d = long.min % 0;
