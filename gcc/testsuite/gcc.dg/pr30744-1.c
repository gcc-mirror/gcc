/* { dg-do compile } */
/* { dg-options "-O2" } */
#include <stddef.h>

typedef __INTPTR_TYPE__ my_intptr_t;

typedef struct {
  my_intptr_t unique;
} G;

void r(G* n)
{
  my_intptr_t p;
  if (((G *) ((void *)((~(my_intptr_t)(p))))) != ((void *)0)) {
    ((G *) ((void *)((~(my_intptr_t)(p)))))->unique = n->unique;
  }
}

