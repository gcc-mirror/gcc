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

/* The xfail for avr, cris-* and pru is due to PR53535.  */
/* { dg-final { scan-tree-dump-not "memset" "optimized" { xfail avr-*-* cris-*-* pru-*-* } } } */
