/* PR target/59874 */
/* { dg-do compile } */
/* { dg-options "-O2 -mlzcnt -masm=att" } */
/* { dg-final { scan-assembler "lzcntw" } } */

unsigned int
foo (unsigned short x)
{
  return x ? __builtin_clz (x) : 16U;
}
