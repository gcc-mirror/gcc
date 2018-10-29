import imports.a14992;  // do not link

int test()
{
    S1 v1;      // OK
    S1* p1;     // OK
    S1[] da1;   // OK
    S1[2] a1;   // OK <- NG

    S2 v2;      // OK
    S2* p2;     // OK
    S2[] da2;   // OK
    S2[2] a2;   // OK <- NG

    return 1;
}
static assert(test());

void main()
{
    test();
}
