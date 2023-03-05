// https://issues.dlang.org/show_bug.cgi?id=23710

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

int main()
{
    assert(test(1) == 0 + 3 + 6);
    return 0;
}
