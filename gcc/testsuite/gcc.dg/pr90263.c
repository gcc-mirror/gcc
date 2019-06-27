/* PR middle-end/90263 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target glibc } */

int *f (int *p, int *q, long n)
{
  return __builtin_mempcpy (p, q, n);
}

/* { dg-final { scan-assembler "mempcpy" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler "memcpy" { target { ! { i?86-*-* x86_64-*-* } } } } } */
