/* { dg-do compile } */
/* { dg-options "-O2" } */
#include <stddef.h>

typedef struct {
  ptrdiff_t unique;
} G;

void r(G* n)
{
  ptrdiff_t p;
  if (((G *) ((void *)((~(ptrdiff_t)(p))))) != ((void *)0)) {
    ((G *) ((void *)((~(ptrdiff_t)(p)))))->unique = n->unique;
  }
}

