/* PR target/59874 */
/* { dg-do compile } */
/* { dg-options "-O2 -mbmi -masm=att" } */
/* { dg-final { scan-assembler "tzcntw" } } */

unsigned int
foo (unsigned short x)
{
  return x ? __builtin_ctz (x) : 16U;
}
