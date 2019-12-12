
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
