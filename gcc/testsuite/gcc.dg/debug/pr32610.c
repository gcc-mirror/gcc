/* PR debug/32610 */
/* { dg-do compile } */

inline void
foo (int x)
{
  double (*arr)[x];
}

void
bar (void)
{
  foo (1);
}
