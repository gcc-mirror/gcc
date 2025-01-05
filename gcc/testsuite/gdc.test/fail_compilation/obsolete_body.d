/*
TEST_OUTPUT:
---
fail_compilation/obsolete_body.d(11): Error: usage of identifer `body` as a keyword is obsolete. Use `do` instead.
---
*/
@__edition_latest_do_not_use
module m;

void test()
in { } body { }
