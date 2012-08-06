/* PR middle-end/46647 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int a;

int
func1 (void)
{
  __builtin_memset (&a, -1, sizeof (a));
  return 0;
}

int
func2 (void)
{
  __builtin_memset (&a, 123, sizeof (a));
  return 0;
}

int
func3 (void)
{
  __builtin_memset (&a, 0, sizeof (a));
  return 0;
}

/* { dg-final { scan-tree-dump-not "memset" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
