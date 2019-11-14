/* PR target/92258 */
/* { dg-do compile } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-masm=intel -msse2" } */

typedef double V __attribute__ ((__vector_size__ (16)));

int
foo (V x, V y)
{
  return __builtin_ia32_ucomisdeq (x, y);
}
