
class C
{
    synchronized void foo()
    {
        foreach(divisor; !(2, 3, 4, 8, 7, 9))
        {
            // ice in ForeachRangeStatement::usesEH()
            foreach (v; 0..uint.max) {}

            // ice in WhileStatement::usesEH()
            while (1) {}
        }
    }
}
