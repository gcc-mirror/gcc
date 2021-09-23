/* PR inline-asm/100785 */

struct S { int a : 1; };

void
foo (struct S *x)
{
  __asm__ ("" : "+m" (x->a));	/* { dg-error "address of bit-field" } */
}

void
bar (struct S *x)
{
  __asm__ ("" : "=m" (x->a));	/* { dg-error "address of bit-field" } */
}

void
baz (struct S *x)
{
  __asm__ ("" : : "m" (x->a));	/* { dg-error "address of bit-field" } */
}
