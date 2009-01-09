/* Test generation of dlmzb for strlen on 405.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mcpu=405" } */
/* { dg-skip-if "other options override -mcpu=405" { ! powerpc_405_nocache } { "*" } { "" } } */

/* { dg-final { scan-assembler "dlmzb\\. " } } */

typedef __SIZE_TYPE__ size_t;

size_t strlen(const char *);

size_t
strlen8(const long long *s)
{
  return strlen((const char *)s);
}
