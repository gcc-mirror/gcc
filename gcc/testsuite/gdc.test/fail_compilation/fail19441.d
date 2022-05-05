// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/fail19441.d(44): Error: cannot use `alias this` to partially initialize variable `wrap[0]` of type `Wrap10595`. Use `wrap[0].i`
---
*/

struct S10595
{
    bool b = true;

    bool test()
    {
        if (!b)  // note: must be a check, not 'return b;'
            return false;

        return true;
    }
}

struct Wrap10595
{
    int i;
    alias i this;
    S10595 s;
}

void main()
{
    {
        Wrap10595[int] wrap;

        wrap[0] = Wrap10595();
        wrap[0].i = 0;

        assert(wrap[0].s.test());  // ok
    }

    {
        Wrap10595[int] wrap;

        wrap[0] = Wrap10595();
        wrap[0] = 0;  // note: using 'alias this' to assign

        assert(wrap[0].s.test());  // failure
    }
}
