/* { dg-additional-options "-march=znver4" { target x86 } } */
/* { dg-require-effective-target avx512vbmi2 { target x86 } } */

int *a;
int b;
long long c, d;
void
e (int f)
{
  for (; f; f++)
    {
      d += (long long)a[f] * b;
      c += (long long)a[f] * 3;
    }
}
