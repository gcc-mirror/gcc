/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* PR tree-optimization/87900 */

/* zeroing out via a CONSTRUCTOR should be treated similarly as a msmet and
   be combined with the malloc below.  */

struct S { int a[1024]; };
struct S *foo ()
{
  struct S *p = (struct S *)__builtin_malloc (sizeof (struct S));
  if (p)
    *p = (struct S){};
  return p;
}

/* { dg-final { scan-tree-dump-times "calloc " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "malloc " "optimized" } } */
/* { dg-final { scan-tree-dump-not "memset " "optimized" } } */
