alias TestType = ubyte;

void test(immutable TestType a, immutable TestType b, TestType c)
{
    switch(c)
    {
        case a: break;
        case (cast(ushort)b): break;
        default: assert(false);
    }
}
