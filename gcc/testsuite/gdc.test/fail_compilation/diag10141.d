/*
TEST_OUTPUT:
---
fail_compilation/diag10141.d(9): Error: module imports.diag10141a import 'unexisting_symbol' not found
---
*/

import imports.diag10141a;
import imports.diag10141a : unexisting_symbol;

Tuple!(int) fun()
{
    return Tuple!(int).init;
}
