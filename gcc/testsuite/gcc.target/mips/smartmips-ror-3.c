/* { dg-do compile } */
/* { dg-options "-msmartmips" } */

#define S 13

NOMIPS16 int rotate_left_constant (unsigned a)
{
  return (a << S) | (a >> (32 - S));
}
/* { dg-final { scan-assembler "\tror\t" } } */
