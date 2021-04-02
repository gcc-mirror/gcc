/*
TEST_OUTPUT:
---
fail_compilation/failinout3748b.d(9): Error: variable `failinout3748b.main.err11` `inout` variables can only be declared inside `inout` functions
---
*/
void main()
{
    inout(int)* err11;
}
