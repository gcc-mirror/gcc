/* PR c++/37561 */
/* { dg-do compile } */

__extension__ __INTPTR_TYPE__ p;
char q;

void
foo ()
{
  ((char *) p)++;	/* { dg-error "lvalue" } */
  ((char *) q)++;	/* { dg-error "lvalue" } */
  ((char *) p)--;	/* { dg-error "lvalue" } */
  ((char *) q)--;	/* { dg-error "lvalue" } */
  ++(char *) p;		/* { dg-error "lvalue" } */
  ++(char *) q;		/* { dg-error "lvalue" } */
  --(char *) p;		/* { dg-error "lvalue" } */
  --(char *) q;		/* { dg-error "lvalue" } */
}

/* { dg-warning "cast to pointer from integer of different size" "" { target *-*-* } 11 } */
/* { dg-warning "cast to pointer from integer of different size" "" { target *-*-* } 13 } */
/* { dg-warning "cast to pointer from integer of different size" "" { target *-*-* } 15 } */
/* { dg-warning "cast to pointer from integer of different size" "" { target *-*-* } 17 } */
