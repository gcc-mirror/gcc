/* { dg-do compile } */
/* { dg-options "-mno-smartmips -march=mips32" } */

NOMIPS16 int rotate_left (unsigned a, unsigned s)
{
  return (a << s) | (a >> (32 - s));
}
/* { dg-final { scan-assembler-not "\tror\t" } } */

