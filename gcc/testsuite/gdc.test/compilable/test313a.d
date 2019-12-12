/*
REQUIRED_ARGS: -de
*/
module test313;

import imports.a313;

void test1()
{
    import imports.b313;
    imports.b313.bug();
}

void test2()
{
    cstdio.printf("");
}

import imports.pkg313.c313;
void test3()
{
    imports.pkg313.c313.bug();
}

template imp()
{
    static import imports.a313templatemixin1;
    import imports.a313templatemixin2;
}

mixin imp!();
void test4()
{
    imports.a313templatemixin1.bug();
    imports.a313templatemixin2.bug();
}
