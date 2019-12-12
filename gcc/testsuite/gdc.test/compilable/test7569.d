template Tuple(T...)
{
    alias T Tuple;
}

void main()
{
    Tuple!(int, int) tup1 = void;
}
