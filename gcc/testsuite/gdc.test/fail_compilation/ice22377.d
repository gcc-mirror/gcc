/*
TEST_OUTPUT:
---
fail_compilation/ice22377.d(8): Error: function `foo` cannot have parameter of type `string` because its linkage is `extern(C++)`
---
*/

extern(C++) void foo(string a) {}
