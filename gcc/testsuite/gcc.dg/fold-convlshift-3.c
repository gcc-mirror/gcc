/* PR middle-end/102029 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
int *
foo (const __PTRDIFF_TYPE__ l)
{
  return (int *) (l << 2);
}
