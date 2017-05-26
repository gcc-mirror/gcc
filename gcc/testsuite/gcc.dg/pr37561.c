/* PR c++/37561 */
/* { dg-do compile } */

__extension__ __INTPTR_TYPE__ p;
char q;

void
foo ()
{
  ((char *) p)++;	/* { dg-error "lvalue" } */

  ((char *) q)++;	/* { dg-error "lvalue" } */
  /* { dg-warning "cast to pointer from integer of different size" "" { target *-*-* } .-1 } */

  ((char *) p)--;	/* { dg-error "lvalue" } */

  ((char *) q)--;	/* { dg-error "lvalue" } */
  /* { dg-warning "cast to pointer from integer of different size" "" { target *-*-* } .-1 } */

  ++(char *) p;		/* { dg-error "lvalue" } */

  ++(char *) q;		/* { dg-error "lvalue" } */
  /* { dg-warning "cast to pointer from integer of different size" "" { target *-*-* } .-1 } */

  --(char *) p;		/* { dg-error "lvalue" } */

  --(char *) q;		/* { dg-error "lvalue" } */
  /* { dg-warning "cast to pointer from integer of different size" "" { target *-*-* } .-1 } */
}

