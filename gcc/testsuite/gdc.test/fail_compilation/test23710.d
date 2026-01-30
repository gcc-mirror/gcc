/* REQUIRED_ARGS: -betterC
TEST_OUTPUT:
---
fail_compilation/test23710.d(112): Error: array concatenation of expression `foo ~ cast(int[])a` requires the GC which is not available with -betterC
---
 */
// https://issues.dlang.org/show_bug.cgi?id=23710

#line 100

int test(int i)
{
    int j;
    int[] foo;
    int[3] a = [1,2,3];
    if (0)
    {
        for (;;)
        {
            import core.stdc.stdio;
            printf("start body\n");
            foo = foo ~ a;
L1:
            printf("foo.length = %zu\n", foo.length);
	    j += foo.length;
            i += 2;
            if (i > 5)
                return j;
            printf("end body\n");
        }
    }
    goto L1;
}
