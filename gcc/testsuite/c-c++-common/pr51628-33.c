/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct pair_t
{
  char x;
  int i[4];
} __attribute__ ((packed, aligned (4)));

extern struct pair_t p;
extern void bar (int *);

void
foo (struct pair_t *p)
{
  bar (p ? p->i : (int *) 0);
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
}
