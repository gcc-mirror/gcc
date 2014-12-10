/* Test generation of mulhhwu. on 440.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=440" } } */
/* { dg-options "-O2 -mcpu=440" } */

/* { dg-final { scan-assembler "mulhhwu\\. " } } */

unsigned int
f(unsigned int b, unsigned int c)
{
  unsigned int a = (b >> 16) * (c >> 16);
  if (!a)
    return 10;
  return a;
}
