/* PR inline-asm/100785 */

struct S { int a : 1; };

void
foo (struct S *x)
{
  __asm__ ("" : "+m" (x->a));	/* { dg-error "address of bit-field" } */
}				/* { dg-error "invalid lvalue in 'asm' output 0" "" { target c } .-1 } */
				/* { dg-error "memory input 1 is not directly addressable" "" { target c } .-2 } */
void
bar (struct S *x)
{
  __asm__ ("" : "=m" (x->a));	/* { dg-error "address of bit-field" } */
}				/* { dg-error "invalid lvalue in 'asm' output 0" "" { target c } .-1 } */

void
baz (struct S *x)
{
  __asm__ ("" : : "m" (x->a));	/* { dg-error "address of bit-field" } */
}				/* { dg-error "memory input 0 is not directly addressable" "" { target c } .-1 } */
