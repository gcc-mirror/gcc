/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -fdump-tree-optimized" } */

#define vect8 __attribute__((vector_size(8) ))
#define vect16 __attribute__((vector_size(16) ))
#define vect32 __attribute__((vector_size(32) ))

vect8 int bar0 (vect8 float a, vect8 float b, vect8 int c)
{
  return (a > b) ? 0 : c;
}

vect16 int bar1 (vect16 float a, vect16 float b, vect16 int c)
{
  return (a > b) ? 0 : c;
}

vect32 int bar2 (vect32 float a, vect32 float b, vect32 int c)
{
  return (a > b) ? 0 : c;
}

/* { dg-final { scan-tree-dump-times ".BIT_ANDN " 3 "optimized" { target { ! ia32 } } } } */
