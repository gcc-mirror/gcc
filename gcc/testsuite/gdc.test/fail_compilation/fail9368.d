// PERMUTE_ARGS:
// REQUIRED_ARGS: -d
/*
TEST_OUTPUT:
---
fail_compilation/fail9368.d(20): Error: enum member `b` not represented in final switch
---
*/

enum E
{
    a,
    b
}

void main()
{
    alias E F;
    F f;
    final switch (f)
    {
        case F.a:
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/fail9368.d(41): Error: enum member `B` not represented in final switch
---
*/

enum G
{
    A,B,C
}

void test286()
{
    G e;
    final switch (e)
    {
        case G.A:
//      case G.B:
        case G.C:
            {}
    }
}

