
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
