/* PR target/82703 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-sra -ftree-vectorize" } */

__attribute__((noinline, noclone)) void
compare (const double *p, const double *q)
{
  for (int i = 0; i < 3; ++i)
    if (p[i] != q[i])
      __builtin_abort ();
}

double vr[3] = { 4, 4, 4 };

int
main ()
{
  double v1[3] = { 1, 2, 3 };
  double v2[3] = { 3, 2, 1 };
  double v3[3];
  __builtin_memcpy (v3, v1, sizeof (v1));
  for (int i = 0; i < 3; ++i)
    v3[i] += v2[i];
  for (int i = 0; i < 3; ++i)
    v1[i] += v2[i];
  compare (v3, vr);
  return 0;
}
