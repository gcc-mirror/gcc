/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O0 -fpic" } */

static struct {
  unsigned short a, b, c, d;
} x[10];

int foo(int i)
{
  return ((*((char *)&x[i] + i)) | (*((char *)&x[i] + i)));
}
