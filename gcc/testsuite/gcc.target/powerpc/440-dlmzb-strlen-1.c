/* Test generation of dlmzb for strlen on 440.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mdejagnu-cpu=440" } */

/* { dg-final { scan-assembler "dlmzb\\. " } } */

typedef __SIZE_TYPE__ size_t;

size_t strlen(const char *);

char s[100] __attribute__ ((aligned (8)));

size_t
strlen8(void)
{
  return strlen(s);
}
