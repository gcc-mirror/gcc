/* PR tree-optimization/127382 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-march=x86-64-v4 -O3 -fdump-tree-vect-details" } */

/* A bit-precise type whose precision fills its container is as good as the
   plain integer type of the same width, so cond_expr_convert_p has to keep
   matching it, no mask packing.  */

/* { dg-final { scan-tree-dump "vect_recog_cond_expr_convert_pattern" "vect" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "kshift" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "kunpck" { target { ! ia32 } } } } */

#if __BITINT_MAXWIDTH__ >= 32
_BitInt(32) a[256];
int b[256];
short d[256];
float c[256];

void
foo (void)
{
  for (int i = 0; i < 256; i++)
    d[i] = c[i] < 1.0f ? (short) a[i] : (short) b[i];
}
#endif
