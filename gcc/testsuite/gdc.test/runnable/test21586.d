/*
RUN_OUTPUT:
---
this
~this 1
---
*/
// https://issues.dlang.org/show_bug.cgi?id=21586

import core.stdc.stdio : printf;

struct S
{
    this(int arg)
    {
        a = arg;
        printf("this\n");
    }

    ~this()
    {
        printf("~this %d\n", a);
    }

    int a;
}

void main()
{
    auto s = true ? S(1) : S(0);
}
