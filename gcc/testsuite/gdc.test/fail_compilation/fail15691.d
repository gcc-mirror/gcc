/*
TEST_OUTPUT:
---
fail_compilation/fail15691.d(15): Error: `c` is not a member of `Foo`
fail_compilation/fail15691.d(20): Error: `bc` is not a member of `Foo`, did you mean variable `abc`?
---
*/

struct Foo { int a; int abc; }

void main()
{
    Foo z = {      // line 13
            a: 3,
            c: 4,  // line 15
        };

    Foo z2 = {     // line 18
            a: 3,
            bc: 4, // line 20
        };
}


