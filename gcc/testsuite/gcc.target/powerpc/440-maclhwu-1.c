/* Test generation of maclhwu on 440.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mdejagnu-cpu=440" } */

/* { dg-final { scan-assembler "maclhwu " } } */

unsigned int
f(unsigned int a, unsigned int b, unsigned int c)
{
  a += (unsigned short)b * (unsigned short)c;
  return a;
}
