/*
TEST_OUTPUT:
---
fail_compilation/fail125.d(15): Error: array index `[2]` is outside array bounds `[0 .. 2]`
fail_compilation/fail125.d(18): Error: template instance `fail125.main.recMove!(1, a, b)` error instantiating
fail_compilation/fail125.d(25):        instantiated from here: `recMove!(0, a, b)`
---
*/


template recMove(int i, X...)
{
    void recMove()
    {
        X[i] = X[i+1];
        // I know the code is logically wrong, should test (i+2 < X.length)
        static if (i+1 < X.length)
            recMove!(i+1, X);
    }
}

void main()
{
    int a, b;
    recMove!(0, a, b);
}
