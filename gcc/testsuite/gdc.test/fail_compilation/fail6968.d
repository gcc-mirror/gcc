// 6968

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
