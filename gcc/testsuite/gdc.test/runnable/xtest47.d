// PERMUTE_ARGS: -unittest
/* TEST_OUTPUT:
---
f
toString
toHash
opCmp
opEquals
Monitor
factory
---
*/

import core.stdc.stdio;

/***************************************************/

void test3()
{
    version (unittest)
    {
        printf("unittest!\n");
    }
    else
    {
        printf("no unittest!\n");
    }

    version (assert)
    {
        printf("assert!\n");
    }
    else
    {
        printf("no assert!\n");
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7983

class A7983 {
        void f() {
                g7983(this);
        }
        unittest {
        }
}

void g7983(T)(T a)
{
        foreach (name; __traits(allMembers, T)) {
                pragma(msg, name);
                static if (__traits(compiles, &__traits(getMember, a, name)))
                {
                }
        }
}

/***************************************************/

int main()
{
    test3();

    return 0;
}
