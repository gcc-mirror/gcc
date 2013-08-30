/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
void abort (void);
__attribute__ ((weak))
int test() 
{
   return 0;
}
static int test2() __attribute__ ((alias("test")));
static int test1() __attribute__ ((weakref)) __attribute__ ((alias("test2")));
static int test4() __attribute__ ((weakref)) __attribute__ ((alias("test")));
main()
{
  test();
  test2();
  test3();
  test4();
}

/* calls to test1 and test2 can be inlined and optmized away. Calls
   to test and test4 are overwritable.  */

/* { dg-final { scan-tree-dump-times "  test " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "  test4 " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "  test1 " "optimized" } } */
/* { dg-final { scan-tree-dump-not "  test2 " "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
