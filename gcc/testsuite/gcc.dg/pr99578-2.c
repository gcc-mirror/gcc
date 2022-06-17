/* PR middle-end/99578 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -Wstringop-overflow" } */

struct S { int a, b[4]; };
struct T { int a, b[8192], c[4]; };

void
foo (struct S *p)
{
  if (p) return;
  __builtin_memset (p->b, 0, sizeof p->b);	/* { dg-warning "writing 16 bytes into a region of size 0 overflows the destination" } */
}

void
bar (struct T *p)
{
  if (p) return;
  __builtin_memset (p->c, 0, sizeof p->c);	/* { dg-warning "writing 16 bytes into a region of size 0 overflows the destination" "" { xfail *-*-* } } */
}

void
baz (void)
{
  __builtin_memset ((void *) 0x8004, 0, 16);	/* { dg-bogus "overflows the destination" } */
}
