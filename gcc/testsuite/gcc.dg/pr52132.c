/* PR debug/52132 */
/* { dg-do compile } */
/* { dg-options "-std=c99 -O2 -g" } */

int l;
void bar (void);

void
foo (int *x, float y)
{
  float b;
  union { float f; int i; } u = { .f = y };
  u.i += 127 << 23;
  u.f = ((-1.0f / 3) * u.f + 2) * u.f - 2.0f / 3;
  b = 0.5 * (u.f + l);
  if (b >= *x)
    bar ();
}
