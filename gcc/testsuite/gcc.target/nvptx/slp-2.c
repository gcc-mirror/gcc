/* { dg-do assemble } */
/* { dg-options "-O2 -ftree-slp-vectorize -save-temps" } */

long long int p[1000] __attribute__((aligned(16)));
long long int p2[1000] __attribute__((aligned(16)));

void __attribute__((noinline, noclone))
foo ()
{
  long long int a, b;

  int i;
  for (i = 0; i < 1000; i += 2)
    {
      a = p[i];
      b = p[i+1];

      p2[i] = a;
      p2[i+1] = b;
    }
}

/* { dg-final { scan-assembler "ld.v2.u64" } } */
/* { dg-final { scan-assembler "st.v2.u64" } } */

