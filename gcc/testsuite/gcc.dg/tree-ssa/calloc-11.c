/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* PR tree-optimization/87900 */

/* zeroing out via a CONSTRUCTOR should be treated similarly as a msmet and
   be combined with the malloc below.  */
typedef int type;

#define size (1025)
type *foo ()
{
  type *p = (type *)__builtin_malloc (size*sizeof(type));
  type tmp[size] = {};
  __builtin_memcpy(p,tmp,sizeof(tmp));
  return p;
}

/* { dg-final { scan-tree-dump-times "calloc " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "malloc " "optimized" } } */
/* { dg-final { scan-tree-dump-not "memset " "optimized" } } */
