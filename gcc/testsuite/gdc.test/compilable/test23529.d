static assert(!__traits(compiles, ()
{
    char[24] x;
    int myNumber = 4;
    return cast(char[4]) (x[myNumber .. myNumber + 4]);
} ()));
