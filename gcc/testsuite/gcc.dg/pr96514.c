/* { dg-do compile } */

int __attribute__ ((pure, returns_twice))
r0 (void);

void
vy (int t7)
{
  while (t7 == 0)
    r0 ();
}

void
qw (int t7)
{
  vy (t7);

  if (0)
    r0 ();
}

void __attribute__ ((simd))
un (int t7)
{
  qw (t7);
  qw (t7);
}
