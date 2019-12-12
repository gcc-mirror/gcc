/*
TEST_OUTPUT:
---
fail_compilation/fail45.d(10): Error: variable fail45.main.O cannot be declared to be a function
---
*/

void main()
{
    typeof(main) O = 0;
}
