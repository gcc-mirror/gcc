/*
TEST_OUTPUT:
---
fail_compilation/ice9254c.d(15): Error: Using the result of a comma expression is not allowed
fail_compilation/ice9254c.d(15): Error: Using the result of a comma expression is not allowed
fail_compilation/ice9254c.d(15): Error: Using the result of a comma expression is not allowed
fail_compilation/ice9254c.d(15): Error: Using the result of a comma expression is not allowed
fail_compilation/ice9254c.d(15): Error: Using the result of a comma expression is not allowed
fail_compilation/ice9254c.d(15): Error: invalid `foreach` aggregate `false`
---
*/

void main()
{
    foreach(divisor; !(2, 3, 4, 8, 7, 9))
    {
        assert(0);

        // ice in ForeachRangeStatement::comeFrom()
        foreach (v; 0..uint.max) {}

        // ice in WhileStatement::comeFrom()
        while (1) {}
    }
}
