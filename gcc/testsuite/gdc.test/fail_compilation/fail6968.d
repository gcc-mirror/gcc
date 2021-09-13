// https://issues.dlang.org/show_bug.cgi?id=6968
/*
TEST_OUTPUT:
---
fail_compilation/fail6968.d(26): Error: cannot pass type `int` as a function argument
fail_compilation/fail6968.d(26): Error: cannot pass type `long` as a function argument
fail_compilation/fail6968.d(26): Error: circular initialization of variable `fail6968.PredAny!(int, long, float).PredAny`
fail_compilation/fail6968.d(31): Error: template instance `fail6968.PredAny!(int, long, float)` error instantiating
fail_compilation/fail6968.d(31):        while evaluating `pragma(msg, PredAny!(int, long, float))`
---
*/

template Pred(A, B)
{
    static if(is(B == int))
        enum bool Pred = true;
    else
        enum bool Pred = false;
}

template PredAny(A, B...)
{
    static if(B.length == 0)
        enum bool PredAny = false;
    else
        enum bool PredAny = Pred(A, B[0]) || PredAny(A, B[1..$]);
}

void main()
{
    pragma(msg, PredAny!(int, long, float));
} 
