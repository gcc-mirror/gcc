/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct pair_t
{
  int x;
  int i;
} __attribute__((packed, aligned (4)));

extern struct pair_t p;
extern int *x;
extern void bar (int *);

int *addr = &p.i;

int *
foo (void)
{
  struct pair_t arr[2] = { { 1, 10 }, { 2, 20 } };
  int *p0, *p1;
  p0 = &arr[0].i;
  bar (p0);
  p1 = &arr[1].i;
  bar (p1);
  bar (&p.i);
  x = &p.i;
  return &p.i;
}
