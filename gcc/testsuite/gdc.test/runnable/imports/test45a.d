module imports.test45a;

import std.stdio;

int foo()
{
    printf("foo()\n");
    return 1;
}


int bar(T)(T t)
{
    printf("bar(t)\n");
    return 3;
}

