/*
TEST_OUTPUT:
---
fail_compilation/ice9254a.d(15): Error: using the result of a comma expression is not allowed
fail_compilation/ice9254a.d(15): Error: using the result of a comma expression is not allowed
fail_compilation/ice9254a.d(15): Error: using the result of a comma expression is not allowed
fail_compilation/ice9254a.d(15): Error: using the result of a comma expression is not allowed
fail_compilation/ice9254a.d(15): Error: using the result of a comma expression is not allowed
fail_compilation/ice9254a.d(15): Error: invalid `foreach` aggregate `false` of type `bool`
---
*/

void main()
{
    foreach (divisor; !(2, 3, 4, 8, 7, 9))
    {
        // ice in ForeachRangeStatement::blockExit()
        foreach (v; 0..uint.max) {}

        // ice in WhileStatement::blockExit()
        while (1) {}
    }
}
