/* PR debug/46893 */
/* { dg-do compile } */
/* { dg-options "-O -g" } */

void
foo (void)
{
  union { unsigned long long l; double d; } u = { 0x7ff0000000000000ULL };
  double v = 0, w = -u.d;

  if (w)
    w = v;
}
