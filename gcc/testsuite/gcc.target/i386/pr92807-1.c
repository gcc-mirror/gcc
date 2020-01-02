/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int
abs2 (unsigned int a) 
{
  unsigned int s = ((a>>15)&0x10001)*0xffff;
  return (a+s)^s;
}

/* { dg-final { scan-assembler-not "leal" } } */
