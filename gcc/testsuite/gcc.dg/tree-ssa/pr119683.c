/* PR tree-optimization/119683 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " = c_\[0-9]*\\\(D\\\) \\\+ \(?:\[0-9-]\)+;" 3 "optimized" } } */

unsigned
foo (signed char c)
{
  if (c >= '0' && c <= '9')
    return c - '0';

  if (c >= 'a' && c <= 'z')
    return c - 'a' + 10;

  if (c >= 'A' && c <= 'Z')
    return c - 'A' + 10;

  return -1;
}
