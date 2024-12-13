/* PR c/48552 */
/* { dg-do compile } */
/* { dg-options "" } */

struct S;

void
f1 (void *x)
{
  __asm ("" : : "r" (*x));	/* { dg-warning "dereferencing" "deref" } */
}				/* { dg-error "invalid use of void expression" "void expr" { target *-*-* } .-1 } */

void
f2 (void *x)
{
  __asm ("" : "=r" (*x));	/* { dg-warning "dereferencing" "deref" } */
}				/* { dg-error "invalid use of void expression" "void expr" { target *-*-* } .-1 } */

void
f3 (void *x)
{
  __asm ("" : : "m" (*x));	/* { dg-warning "dereferencing" } */
  /* { dg-error "memory input 0 is not directly addressable" "not addressable" { target *-*-* } .-1 } */
}

void
f4 (void *x)
{
  __asm ("" : "=m" (*x));	/* { dg-warning "dereferencing" } */
}

void
f5 (void *x)
{
  __asm ("" : : "g" (*x));	/* { dg-warning "dereferencing" "deref" } */
}				/* { dg-error "invalid use of void expression" "void expr" { target *-*-* } .-1 } */

void
f6 (void *x)
{
  __asm ("" : "=g" (*x));	/* { dg-warning "dereferencing" "deref" } */
}				/* { dg-error "invalid use of void expression" "void expr" { target *-*-* } .-1 } */

void
f7 (struct S *x)
{
  __asm ("" : : "r" (*x));	/* { dg-error "invalid use of undefined type" } */
}

void
f8 (struct S *x)
{
  __asm ("" : "=r" (*x));	/* { dg-error "impossible constraint in 'asm'" } */
  /* { dg-error "non-memory output 0 must stay in memory" "memory" { target *-*-* } .-1 } */
}
