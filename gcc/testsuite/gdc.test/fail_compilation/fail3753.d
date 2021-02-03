/*
TEST_OUTPUT:
---
Error: cannot mix core.std.stdlib.alloca() and exception handling in _Dmain()
---
*/

import core.stdc.stdlib : alloca;
import core.stdc.stdio;

struct TheStruct
{
    ~this()
    {
        printf("dtor()\n");
    }
}

void bar()
{
    printf("bar()\n");
}

void main()
{
    auto s = TheStruct();
    bar();
    auto a = alloca(16);
    printf("test()\n");
    version (DigitalMars)
    {
        version (Win32) static assert(0);
        version (linux)
        {
            static assert(0);
        }
        version (FreeBSD)
        {
            static assert(0);
        }
        version (OSX)
        {
            static assert(0);
        }
    }
    else
        static assert(0);
}
