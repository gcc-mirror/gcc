/*
TEST_OUTPUT:
---
fail_compilation/ice10341.d(10): Error: case range not in switch statement
---
*/

void main()
{
    case 1: .. case 2:
}
