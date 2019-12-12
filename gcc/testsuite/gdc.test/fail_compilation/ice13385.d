/*
TEST_OUTPUT:
---
fail_compilation/ice13385.d(9): Error: protection attribute 'package(a)' does not bind to one of ancestor packages of module `ice13385`
---
*/
module ice13385;

package(a) void foo() {}
