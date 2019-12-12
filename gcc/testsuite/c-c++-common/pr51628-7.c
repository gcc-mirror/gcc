/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct pair_t
{
  int x;
  int i[4];
} __attribute__((packed, aligned (4)));

extern struct pair_t p;
extern int *x;
extern void bar (int *);

int *addr = p.i;

int *
foo (struct pair_t *p)
{
  int *p0, *p1;
  p0 = p->i;
  bar (p0);
  p1 = &p->i[1];
  bar (p1);
  bar (p->i);
  bar (&p->i[2]);
  x = p->i;
  return &p->i[3];
}
