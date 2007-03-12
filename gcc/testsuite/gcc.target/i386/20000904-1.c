/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O0 -fpic" } */

static struct {
  unsigned short a, b, c, d;
} x[10];

int foo(int i)
{
  return ((*((char *)&x[i] + i)) | (*((char *)&x[i] + i)));
}
