/* { dg-do compile } */
/* { dg-options "-O2 -march=r2" } */

/* Test generation of Nios II R2 "andci" and "andchi" instructions.  */

unsigned int f (unsigned int a)
{
  return a & 0xfffffff0;
}

unsigned int g (unsigned int b)
{
  return b & 0xfff0ffff;
}

/* { dg-final { scan-assembler "\tandci\t.*" } }  */
/* { dg-final { scan-assembler "\tandchi\t.*" } }  */

