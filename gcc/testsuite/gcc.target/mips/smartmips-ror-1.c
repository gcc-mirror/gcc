/* { dg-do compile } */
/* { dg-mips-options "-O -msmartmips" } */

#define NOMIPS16 __attribute__ ((nomips16)) 

NOMIPS16 int rotate_left (unsigned a, unsigned s)
{
  return (a << s) | (a >> (32 - s));
}
/* { dg-final { scan-assembler "\tror\t" } } */
