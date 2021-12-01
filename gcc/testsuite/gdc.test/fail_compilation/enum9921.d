/*
TEST_OUTPUT:
---
fail_compilation/enum9921.d(9): Error: enum `enum9921.X` base type must not be `void`
fail_compilation/enum9921.d(11): Error: enum `enum9921.Z` base type must not be `void`
---
*/

enum X : void;

enum Z : void { Y };
