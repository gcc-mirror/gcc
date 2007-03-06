/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef struct {
  unsigned long unique;
} G;

void r(G* n)
{
  unsigned long p;
  if (((G *) ((void *)((~(unsigned long)(p))))) != ((void *)0)) {
    ((G *) ((void *)((~(unsigned long)(p)))))->unique = n->unique;
  }
}

