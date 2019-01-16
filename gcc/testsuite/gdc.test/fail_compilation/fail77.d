void test()
{
    int i;
    ubyte[4] ub;
    ub[] = cast(ubyte[4]) &i;
    //ub[] = (cast(ubyte*) &i)[0..4];
}

