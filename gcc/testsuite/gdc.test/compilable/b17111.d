alias TestType = ubyte;

void test()
{
    TestType a,b,c;

    switch(c)
    {
        case a: break;
        case (cast(ushort)b): break;
        default: assert(false);
    }
}
