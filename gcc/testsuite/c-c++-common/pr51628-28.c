/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct A { 
  int i;
} __attribute__ ((packed));

int *
foo3 (struct A *p1, int *q1, int *q2, struct A *p2) 
{
  return (q1 
	  ? &p1->i
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
	  : (q2 ? &p2->i : q2));
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
}

int*
foo4 (struct A *p1, int **q1, int *q2, int *q3, struct A *p2)
{
  return (q1
	  ? (*q1 = q2, &p1->i)
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
	  : (q2
	     ? (*q1 = &p1->i,
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
		*q2 = 2, &p2->i)
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
	     : q2));
}
