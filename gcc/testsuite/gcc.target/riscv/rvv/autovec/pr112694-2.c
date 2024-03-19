/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve64d_zvfh_zfh -mabi=lp64d -O3 -fno-vect-cost-model" } */

long a[100], b[100], c[100];

void g1 ()
{
  for (int i = 0; i < 100; i += 2)
    {
      c[i] += a[b[i]] + 1;
      c[i + 1] += a[b[i + 1]] + 2;
    }
}

long g2 ()
{
  long res = 0;
  for (int i = 0; i < 100; i += 2)
    {
      res += a[b[i + 1]];
      res += a[b[i]];
    }
  return res;
}

long g3 ()
{
  long res = 0;
  for (int i = 0; i < 100; i += 2)
    {
      res += a[b[i]];
      res += a[b[i + 1]];
    }
  return res;
}
