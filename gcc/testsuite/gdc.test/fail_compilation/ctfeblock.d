/* TEST_OUTPUT:
---
fail_compilation/ctfeblock.d(112): Error: cannot `goto` into `if (__ctfe)` block
---
*/

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=18472
// https://github.com/dlang/dmd/pull/14676

#line 100

struct T { }

@nogc void test1()
{
    int a;
    if (__ctfe)
    {
L1:
        new T();
	a = 3;
    }
    goto L1;
}

@nogc void test2()
{
    if (__ctfe)
    {
        new T();
    }
}
