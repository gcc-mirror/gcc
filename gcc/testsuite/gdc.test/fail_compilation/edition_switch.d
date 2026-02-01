/*
REQUIRED_ARGS: -edition=2024
TEST_OUTPUT:
---
fail_compilation/edition_switch.d(10): Error: usage of identifer `body` as a keyword is obsolete. Use `do` instead.
---
*/

void test()
in { } body { }
