/* { dg-do compile } */
/* { dg-mips-options "-O -msmartmips -mno-mips16" } */

#define S 13

int rotate_right_constant (unsigned a)
{
  return (a >> S) | (a << (32 - S));
}
/* { dg-final { scan-assembler "\tror\t" } } */
