/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
static int a=4;
static int b __attribute__ ((alias("a")));
int
main()
{
   return b+a;
}
/* { dg-final { scan-tree-dump "return 8" "optimized" } } */
