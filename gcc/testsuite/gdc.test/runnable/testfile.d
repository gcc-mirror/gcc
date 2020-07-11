// RUNNABLE_PHOBOS_TEST
// PERMUTE_ARGS:

import std.file;
import std.stdio;

/***********************************************/

void test1()
{
    auto p = std.file.getcwd();

    writefln("%s '%s'\n", p.length, p);
    assert(p[$ - 1] != 0);
}

/***********************************************/

int main()
{
    test1();

    printf("Success\n");
    return 0;
}
