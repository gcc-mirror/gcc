/* PR middle-end/79788 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

long long
foo (long long x, long long y)
{
  if (y > 1234567891234567891234567891234567812 / x)	/* { dg-warning "integer constant is too large for its type" } */
    return x;
  return 0;
}
