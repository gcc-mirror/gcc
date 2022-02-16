/*
TEST_OUTPUT:
---
fail_compilation/traits_parameters.d(9): Error: `__traits(parameters)` cannot have arguments, but `234` was supplied
fail_compilation/traits_parameters.d(10): Error: `__traits(parameters)` may only be used inside a function
---
*/

typeof(__traits(parameters, 234)) xyz;
typeof(__traits(parameters)) x;
