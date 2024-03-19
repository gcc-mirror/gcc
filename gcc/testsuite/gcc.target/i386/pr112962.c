/* PR target/112962 */
/* { dg-do compile } */
/* { dg-options "-fexceptions -mssse3" } */

typedef int __attribute__((__vector_size__ (16))) V;

void
foo (void)
{
  __builtin_ia32_pabsd128 ((V) {});
}
