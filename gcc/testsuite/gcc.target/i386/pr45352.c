/* { dg-do compile } */
/* { dg-options "-O3 -march=amdfam10 -fselective-scheduling2 -fsel-sched-pipelining -funroll-all-loops" } */

struct S
{
  struct
  {
    int i;
  } **p;
  int x;
  int y;
};

extern int baz (void);
extern int bar (void *, int, int);

void
foo (struct S *s)
{
  int i;
  for (i = 0; i < s->x; i++)
    bar (s->p[i], baz (), s->y);
  for (i = 0; i < s->x; i++)
    s->p[i]->i++;
}
