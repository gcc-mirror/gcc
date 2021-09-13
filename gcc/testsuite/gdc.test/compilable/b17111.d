/*
TEST_OUTPUT:
---
compilable/b17111.d(16): Deprecation: `case` variables have to be `const` or `immutable`
compilable/b17111.d(17): Deprecation: `case` variables have to be `const` or `immutable`
---
*/
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
