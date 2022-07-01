/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */


__attribute__((noinline))
static int
test2(int a)
{
        if (__builtin_constant_p (a))
                __builtin_abort ();
        return a;
}
static int
test(int *a)
{
        int val = *(volatile int *)a;
        if (__builtin_constant_p (val))
                __builtin_abort ();
        if (val)
          return test2(val);
        return 0;
}
int a;
int
main()
{
        a = 0;
        return test (&a);
}
/* { dg-final { scan-tree-dump "test2" "optimized" } } */
