/* REQUIRED_ARGS: -betterC
TEST_OUTPUT:
---
fail_compilation/test23710.d(111): Error: array concatenation of expression `foo ~ [1, 2, 3]` requires the GC which is not available with -betterC
---
 */
// https://issues.dlang.org/show_bug.cgi?id=23710

#line 100

int test(int i)
{
    int j;
    int[] foo;
    if (0)
    {
        for (;;)
        {
            import core.stdc.stdio;
            printf("start body\n");
            foo = foo ~ [1,2,3];
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
