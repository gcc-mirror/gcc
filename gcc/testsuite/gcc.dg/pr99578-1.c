/* PR middle-end/99578 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -Warray-bounds" } */

struct S { int a, b[4]; };
struct T { int a, b[8192], c[4]; };

void
foo (struct S *p)
{
  if (p) return;
  __builtin_memset (p->b, 0, sizeof p->b);	/* { dg-warning "offset \\\[0, 15\\\] is out of the bounds \\\[0, 0\\\]" } */
}

void
bar (struct T *p)
{
  if (p) return;
  __builtin_memset (p->c, 0, sizeof p->c);	/* { dg-warning "offset \\\[0, 15\\\] is out of the bounds \\\[0, 0\\\]" "" { xfail *-*-* } } */
}

void
baz (void)
{
  __builtin_memset ((void *) 0x8004, 0, 16);	/* { dg-bogus "is out of the bounds" } */
}
