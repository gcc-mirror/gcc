// https://issues.dlang.org/show_bug.cgi?id=751
// Compiler segfault on template expansion

extern(C) int printf(const char*, ...);

template TypeTuple( TList... )
{
    alias TList TypeTuple;
}

template IndexOf( T, TList... )
{
    static if( TList.length == 0 )
        const size_t IndexOf = 1;
    else static if( is( T == typeof( TList[0] ) ) )
        const size_t IndexOf = 0;
    else
        const size_t IndexOf = 1 + IndexOf!( T, (TList[1 .. $]) );
}

void main()
{
    TypeTuple!(int, long) T;
    printf( "%u\n", cast(uint)IndexOf!(long, T) );
}
