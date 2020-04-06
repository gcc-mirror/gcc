void main()
{
    static assert(__traits(isSame, int, int));
    static assert(__traits(isSame, int[][], int[][]));
    static assert(__traits(isSame, bool*, bool*));

    static assert(!__traits(isSame, bool*, bool[]));
    static assert(!__traits(isSame, float, double));
}
