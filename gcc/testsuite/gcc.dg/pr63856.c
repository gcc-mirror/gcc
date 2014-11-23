/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi" } */
/* { dg-additional-options "-fPIC" { target fpic } } */
typedef int v2si __attribute__ ((vector_size (8)));
typedef short v4hi __attribute__ ((vector_size (8)));

int __attribute__ ((noinline, noclone)) f (v2si A, int N)
{
  return ((v4hi) A)[N];
}

int __attribute__ ((noinline, noclone)) g (v2si A, int N)
{
  return ((v4hi) A)[N];
}
