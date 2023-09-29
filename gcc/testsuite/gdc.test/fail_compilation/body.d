/* REQUIRED_ARGS: -wo -w
TEST_OUTPUT:
---
fail_compilation/body.d(11): Warning: usage of identifer `body` as a keyword is obsolete. Use `do` instead.
Error: warnings are treated as errors
       Use -wi if you wish to treat warnings only as informational.
---
*/

void test()
in { } body { }
