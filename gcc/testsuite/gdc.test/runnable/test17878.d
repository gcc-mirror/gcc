@__future int foo()
{
    return 0;
}

int bar()
{
    return 1;
}

@__future int c;


void main()
{
    static assert(__traits(isFuture, foo));
    static assert(!__traits(isFuture, bar));
    static assert(__traits(isFuture, c));
}
