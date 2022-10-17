module imports.test45b;

import core.stdc.stdio;

int foo(int i)
{
    printf("foo(int)\n");
    return 2;
}

int bar(T)(T t, int i)
{
    printf("bar(t,i)\n");
    return 4;
}
