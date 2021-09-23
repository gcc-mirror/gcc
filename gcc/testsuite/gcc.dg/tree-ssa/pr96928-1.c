/* PR tree-optimization/96928 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " = a_\[0-9]*\\\(D\\\) >> " 5 "phiopt2" } } */
/* The following check is done at optimized because a ^ (~b) is rewritten as ~(a^b)
   and in the case of match.pd optimizing these ?:, the ~ is moved out already
   by the time we get to phiopt2. */
/* { dg-final { scan-tree-dump-times "c_\[0-9]*\\\(D\\\) \\\^" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " = ~" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " = \[abc_0-9\\\(\\\)D]* \\\^ " 5 "phiopt2" } } */
/* { dg-final { scan-tree-dump-not "a < 0" "phiopt2" } } */

int
foo (int a)
{
  if (a < 0)
    return ~a;
  return a;
}

int
bar (int a, int b)
{
  if (a < 0)
    return ~b;
  return b;
}

unsigned
baz (int a, unsigned int b)
{
  if (a < 0)
    return ~b;
  return b;
}

unsigned
qux (int a, unsigned int c)
{
  if (a >= 0)
    return ~c;
  return c;
}

int
corge (int a, int b)
{
  if (a >= 0)
    return b;
  return ~b;
}
