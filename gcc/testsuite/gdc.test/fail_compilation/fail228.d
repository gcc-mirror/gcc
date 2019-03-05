/*
TEST_OUTPUT:
---
fail_compilation/fail228.d(22): Error: undefined identifier `localVariable`
---
*/

//import core.stdc.stdio : printf;

int ToTypeString(T : int)()
{
    return 1;
}

int ToTypeString(T : string)()
{
    return 2;
}

void main()
{
    auto x = ToTypeString!(typeof(localVariable))();
}
