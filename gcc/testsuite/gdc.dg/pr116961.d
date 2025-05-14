// { dg-do compile }
struct S116961
{
    float thing = 0.0;
}

static assert(__traits(isZeroInit, S116961) == true);
