/* Test generation of mulchwu on 405.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mdejagnu-cpu=405" } */
/* { dg-skip-if "other options override -mcpu=405" { ! powerpc_405_nocache } } */

/* { dg-final { scan-assembler "mulchwu " } } */

int
f(unsigned int b, unsigned int c)
{
  unsigned int a = (unsigned short)b * (c >> 16);
  return a;
}
