// REQUIRED_ARGS: -o- -w

/*
TEST_OUTPUT:
---
fail_compilation/warn14905.d(16): Warning: statement is not reachable in template instance warn14905.fun!"a".fun
fail_compilation/warn14905.d(16): Warning: statement is not reachable in template instance warn14905.fun!"b".fun
Error: warnings are treated as errors
       Use -wi if you wish to treat warnings only as informational.
---
*/

bool fun(string s)()
{
    return true;
    return false;
}

void main()
{
    cast(void)fun!"a";
    cast(void)fun!"b";
}
