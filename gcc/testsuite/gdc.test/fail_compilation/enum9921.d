/*
TEST_OUTPUT:
---
fail_compilation/enum9921.d(9): Error: enum `enum9921.X` base type must not be `void`
fail_compilation/enum9921.d(11): Error: enum `enum9921.Z` base type must not be `void`
fail_compilation/enum9921.d(13): Error: variable `enum9921.x` - manifest constants must have initializers
---
*/
enum X : void;

enum Z : void { Y };

enum int x;
