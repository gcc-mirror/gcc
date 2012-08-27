/* { dg-do compile } */
/* { dg-options "-msmartmips" } */

NOMIPS16 int rotate_left (unsigned a, unsigned s)
{
  return (a << s) | (a >> (32 - s));
}
/* { dg-final { scan-assembler "\tror\t" } } */
