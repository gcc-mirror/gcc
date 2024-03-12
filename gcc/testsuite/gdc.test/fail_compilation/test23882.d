// https://issues.dlang.org/show_bug.cgi?id=23882

/*
TEST_OUTPUT:
---
fail_compilation/test23882.d(26): Error: `typeof((*YC).S).init` is used as a type
---
*/

struct G(H)
{
    Tuple!(R) S;
}

struct BB(H)
{
    H* YC;
    alias YC this;
}

struct R
{
    BB!(G!float) CB;
    alias CB this;

    this(typeof(CB.S).init);
}

struct Tuple(Specs)
{
    Specs expand;

    this(Specs values)
    {
        expand = values;
    }
}
