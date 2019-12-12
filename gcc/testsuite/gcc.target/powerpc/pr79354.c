/* PR target/79354 */
/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */
/* { dg-final { scan-assembler-not {\mstxssp\M} } } */

int b, f, g;
float e;
unsigned long d;

void
foo (int *a)
{
  for (g = 0; g < 32; g++)
    if (f)
      {
        e = d;
        __builtin_memcpy (&b, &e, sizeof (float));
        b = *a;
      }
}
