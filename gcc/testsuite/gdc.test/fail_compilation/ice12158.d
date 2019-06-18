/*
TEST_OUTPUT:
---
fail_compilation/ice12158.d(7): Error: module `object` import `nonexisting` not found
---
*/
import object : nonexisting;
auto x = nonexisting.init;
