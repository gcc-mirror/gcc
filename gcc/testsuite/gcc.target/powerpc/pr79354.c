/* PR target/79354 */
/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */
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
