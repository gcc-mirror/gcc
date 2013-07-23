/* Test C11 _Generic.  Error cases.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

struct incomplete;

void
f (int n)
{
  /* Multiple 'default's.  */
  _Generic (n, default: 1, default: 2); /* { dg-error "duplicate .*default.* case" } */

  /* Variably-modified type not ok.  */
  _Generic (n, int[n]: 0, default: 1);	/* { dg-error "variable length type" } */
  /* Type must be complete.  */
  _Generic (n, struct incomplete: 0, default: 1); /* { dg-error "incomplete type" } */
  _Generic (n, void: 0, default: 1); /* { dg-error "incomplete type" } */

  /* Type must be object type.  */
  _Generic (n, void (void): 0, default: 1); /* { dg-error "function type" } */

  /* Two compatible types in association list.  */
  _Generic (&n, int: 5, signed int: 7, default: 23); /* { dg-error "two compatible types" } */

  /* No matching association.  */
  _Generic (n, void *: 5);	/* { dg-error "not compatible with any association" } */
}
