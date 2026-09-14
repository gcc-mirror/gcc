/* PR tree-optimization/127379 */
/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-additional-options "-Wno-psabi" } */

__attribute__((vector_size (sizeof (unsigned))))
  unsigned v1;
int a;

int
f (void)
{
  return (a | (v1 | -v1) >> 31)[0];
}

typedef unsigned int vu __attribute__ ((vector_size (16)));
typedef int vi __attribute__ ((vector_size (16)));

vu
g (vu x)
{
  return (x | -x) >> 31;
}

vu
h (vi x)
{
  return (vu) (x | -x) >> 31;
}
