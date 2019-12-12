/* { dg-do compile } */
/* { dg-options "-Og -fexpensive-optimizations -fno-tree-fre -g --param=max-combine-insns=4" } */

unsigned a, b, c;
void
foo (void)
{
  unsigned short e;
  __builtin_mul_overflow (0, b, &a);
  __builtin_sub_overflow (59347, 9, &e);
  e <<= a & 5;
  c = e;
}
