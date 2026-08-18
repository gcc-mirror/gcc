/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -Wno-psabi" } */

typedef int v4si __attribute__ ((vector_size (16)));
typedef unsigned int v4ui __attribute__ ((vector_size (16)));

int
eq (unsigned int a, unsigned int b)
{
  return a / b * b == a;
}

int
ne (unsigned int a, unsigned int b)
{
  return a / b * b != a;
}

int
nes (int a, int b)
{
  return a / b * b != a;
}

v4si
veq (v4ui a, v4ui b)
{
  return a / b * b == a;
}

v4si
vne (v4ui a, v4ui b)
{
  return a / b * b != a;
}

/* { dg-final { scan-tree-dump-not " \\* " "optimized" } } */
/* { dg-final { scan-tree-dump " % " "optimized" } } */
