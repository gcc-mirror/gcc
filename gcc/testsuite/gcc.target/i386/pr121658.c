/* PR target/121658 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mgfni" } */

__attribute__((__vector_size__(64))) unsigned char v;

void
foo (void)
{
  v = (v << 7) | (v >> 1);
}
