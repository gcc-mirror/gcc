/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O -Wno-incompatible-pointer-types" } */

struct __attribute__((packed)) S { char p; int a, b, c; };

short *
baz (int x, struct S *p)
{
  return (x
	  ? &p->a 
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
	  : &p->b);
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
}

short *
qux (int x, struct S *p)
{
  return (short *) (x
		    ?  &p->a
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
		    : &p->b);
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
}
