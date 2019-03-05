// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/fail12567.d(8): Error: string expected, not '"a" ~ "b"'
---
*/
deprecated("a" ~ "b") module fail12567;
