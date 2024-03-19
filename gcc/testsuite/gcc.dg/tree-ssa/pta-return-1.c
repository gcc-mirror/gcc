/* PR112653 */
/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

char *test;
char *
copy_test ()
{
  char *test2 = __builtin_malloc (1000);
  __builtin_memmove (test2, test, 1000);
  return test2;
}

/* We should be able to turn the memmove into memcpy by means of alias
   analysis.  */
/* { dg-final { scan-tree-dump "memcpy" "optimized" } } */
