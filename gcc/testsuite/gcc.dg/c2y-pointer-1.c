/* Test C2y constraints on pointer conversions.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

#include <stddef.h>
#include <stdint.h>

void *p1;
struct s { int a; } s1;
struct sp { void *p; } s2;
union t { int b; } t1;
union tp { void *p; } t2;

void
f ()
{
  (double) p1; /* { dg-error "pointer value used where a floating-point was expected" } */
  (struct s) p1; /* { dg-error "conversion to non-scalar type requested" } */
  (union t) p1; /* { dg-error "cast to union type from type not present in union" } */
  (struct sp) p1; /* { dg-error "conversion to non-scalar type requested" } */
  (union tp) p1; /* { dg-error "ISO C forbids casts to union type" } */
  (nullptr_t) p1; /* { dg-error "conversion" } */
  /* { dg-message "or a null pointer constant can be converted" "conversion note" { target *-*-* } .-1 } */
  (intptr_t) p1;
  (int *) p1;
  (void) p1;
  (void *) 0.0; /* { dg-error "cannot convert to a pointer type" } */
  (void *) s1; /* { dg-error "cannot convert to a pointer type" } */
  (void *) t1; /* { dg-error "cannot convert to a pointer type" } */
  (void *) s2; /* { dg-error "cannot convert to a pointer type" } */
  (void *) t2; /* { dg-error "cannot convert to a pointer type" } */
  (void *) nullptr;
  (void *) 0;
  (void *) (int *) 0;
  (void *) (void) p1; /* { dg-error "invalid use of void expression" } */
}

void
g ()
{
  double d = p1; /* { dg-error "incompatible types" } */
  s1 = p1; /* { dg-error "incompatible types" } */
  t1 = p1; /* { dg-error "incompatible types" } */
  s2 = p1; /* { dg-error "incompatible types" } */
  t2 = p1; /* { dg-error "incompatible types" } */
  nullptr_t np = p1; /* { dg-error "incompatible types" } */
  p1 = 0.0; /* { dg-error "incompatible types" } */
  p1 = s1; /* { dg-error "incompatible types" } */
  p1 = t1; /* { dg-error "incompatible types" } */
  p1 = s2; /* { dg-error "incompatible types" } */
  p1 = t2; /* { dg-error "incompatible types" } */
  p1 = (void) p1; /* { dg-error "invalid use of void expression" } */
}
