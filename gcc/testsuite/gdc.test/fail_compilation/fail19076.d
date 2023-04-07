/*
TEST_OUTPUT:
---
fail_compilation/fail19076.d(11): Error: no property `V` for type `fail19076.I`
fail_compilation/fail19076.d(11): Error: `(I).V` cannot be resolved
---
*/

interface P { }
interface I : P { }
auto F = __traits(getVirtualMethods, I, "V");
