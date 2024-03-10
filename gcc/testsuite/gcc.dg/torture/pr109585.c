/* { dg-do run } */

#include <stdlib.h>

struct P {
    long v;
    struct P *n;
};

struct F {
    long x;
    struct P fam[];
};

int __attribute__((noipa))
f(struct F *f, int i)
{
  struct P *p = f->fam;
  asm("" : "+r"(f): "r"(p));
  p->v = 0;
  p->n = 0;
  return f->fam->n != 0;
}

int
main()
{
  struct F *m = malloc (sizeof (long) + 2 * sizeof (struct P));
  m->fam[0].n = &m->fam[1];
  if (f (m, 0))
    abort ();
  return 0;
}
