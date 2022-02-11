/*
EXTRA_SOURCES: imports/a21a.d
PERMUTE_ARGS:
RUN_OUTPUT:
---
goodFunc
badFunc
---
*/

import core.stdc.stdio;
import imports.a21a;


template BadMixin()
{
    int badFunc()
    {
        printf("badFunc\n");
        return 2;
    }
}


int main()
{
    int i;
    auto x = new SomeClass;
    i = x.goodFunc();
    assert(i == 1);
    i = x.badFunc();
    assert(i == 2);

    return 0;
}
