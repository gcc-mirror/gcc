/* { dg-do compile } */
/* { dg-mips-options "-O -msmartmips" } */

#define S 13

NOMIPS16 int rotate_right_constant (unsigned a)
{
  return (a >> S) | (a << (32 - S));
}
/* { dg-final { scan-assembler "\tror\t" } } */
