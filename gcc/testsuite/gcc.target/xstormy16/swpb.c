/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned short foo(unsigned short x)
{
  return (x>>8) | (x<<8);
}

/* { dg-final { scan-assembler "swpb r2" } } */
