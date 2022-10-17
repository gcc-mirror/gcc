/*
REQUIRED_ARGS: -betterC
RUN_OUTPUT:
---
init
init
main
fini
fini
---
*/

import core.stdc.stdio;

extern(C):

pragma(crt_constructor)
void init()
{
    puts("init");
}

pragma(crt_destructor)
void fini2()
{
    puts("fini");
}

pragma(crt_constructor)
void foo()
{
    puts("init");
}

pragma(crt_destructor)
void bar()
{
    puts("fini");
}

int main()
{
    puts("main");
    return 0;
}
