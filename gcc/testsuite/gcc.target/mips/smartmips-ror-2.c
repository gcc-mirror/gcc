/* { dg-do compile } */
/* { dg-options "-O -msmartmips" } */

NOMIPS16 int rotate_right (unsigned a, unsigned s)
{
  return (a >> s) | (a << (32 - s));
}
/* { dg-final { scan-assembler "\tror\t" } } */
