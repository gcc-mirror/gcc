/*
TEST_OUTPUT:
---
fail_compilation/fail20658.d(14): Error: modifying field `U.m` which overlaps with fields with other storage classes is not allowed in a `@safe` function
---
*/

union U
{
    int m;
    immutable int i;
}
U u;
enum e = () @safe { u.m = 13; };
