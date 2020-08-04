/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps" } */

unsigned short umulhi3_highpart(unsigned short x, unsigned short y)
{
  return ((unsigned int)x * (unsigned int)y) >> 16;
}

unsigned int umulsi3_highpart(unsigned int x, unsigned int y)
{
  return ((unsigned long)x * (unsigned long)y) >> 32;
}

/* { dg-final { scan-assembler-times "mul.hi.u16" 1 } } */
/* { dg-final { scan-assembler-times "mul.hi.u32" 1 } } */
