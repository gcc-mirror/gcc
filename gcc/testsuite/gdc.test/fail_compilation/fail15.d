/*
TEST_OUTPUT:
---
fail_compilation/fail15.d(24): Error: cannot use `[]` operator on expression of type `void`
---
*/
/*
Segfault on DMD 0.095
http://www.digitalmars.com/d/archives/digitalmars/D/bugs/926.html
*/
module test;

template Test()
{
    bool opIndex(bool x)
    {
        return !x;
    }
}

void main()
{
    mixin Test!() xs;
    bool x = xs[false];
}


