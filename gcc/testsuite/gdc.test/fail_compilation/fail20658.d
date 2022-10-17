/*
TEST_OUTPUT:
---
fail_compilation/fail20658.d(14): Error: field `U.m` cannot modify fields in `@safe` code that overlap fields with other storage classes
---
*/

union U
{
    int m;
    immutable int i;
}
U u;
enum e = () @safe { u.m = 13; };
