void foo(int i)
in
{
    class X1
    {
        void in_nested() pure
        in { assert(i); }   // OK <- NG
        out { assert(i); }  // OK <- NG
        do {}
    }
}
out
{
    class X2
    {
        void out_nested() pure
        in { assert(i); }   // OK <- NG
        out { assert(i); }  // OK <- NG
        do {}
    }
}
do
{
}
