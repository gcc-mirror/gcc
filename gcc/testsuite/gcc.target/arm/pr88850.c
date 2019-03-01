/* PR target/88850 */
/* { dg-do compile } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } {"-mfloat-abi=softfp" } } */
/* { dg-options "-O2 -march=armv7-a -mfloat-abi=softfp -mfpu=neon -fdump-rtl-final" } */
/* { dg-require-effective-target arm_neon_ok } */

typedef __builtin_neon_qi int8x8_t __attribute__ ((__vector_size__ (8)));
void bar (int8x8_t, int8x8_t);

void
foo (int8x8_t x, int8x8_t y)
{
 bar (y, x);
}

void foo2 (int8x8_t x)
{
  int8x8_t y;
  bar (y, x);
}

/* Check that these 64-bit moves are done in SI.  */
/* { dg-final { scan-rtl-dump "_movsi_vfp" "final" } } */
