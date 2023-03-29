/* Test for compound literals: in C99 only.  Test for invalid uses.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

struct s { int a; int b; };
union u { int c; int d; };

struct si;
union ui;

void
foo (int a)
{
  /* The type name must not be incomplete (apart from arrays of unknown
     size), or a function type, or a VLA type.  */
  (void) { 1 }; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "init" "void type" { target *-*-* } .-1 } */
  &(struct si) { 1 }; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "init" "incomplete struct type" { target *-*-* } .-1 } */
  /* { dg-error "invalid use of undefined type" "" { target *-*-* } .-2 } */
  &(union ui) { 1 }; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "init" "incomplete union type" { target *-*-* } .-1 } */
  /* { dg-error "invalid use of undefined type" "" { target *-*-* } .-2 } */
  (void (void)) { 0 }; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "compound literal has function type" "function type" { target *-*-* } .-1 } */
  (int [a]) { 1 }; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "init|variable" "VLA type" { target *-*-* } .-1 } */
  /* Initializers must not attempt to initialize outside the object
     declared.  */
  (int [1]) { [1] = 2 }; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "init" "value outside array" { target *-*-* } .-1 } */
  (int [1]) { [-1] = 2 }; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "init" "value outside array" { target *-*-* } .-1 } */
  (int [1]) { 0, 1 }; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "init" "value outside array" { target *-*-* } .-1 } */
}

int z;

/* Outside a function, initializers must be constant.  */
struct s *s0 = &(struct s) { 0, z }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "init" "non-const" { target *-*-* } .-1 } */
int sz = sizeof((struct s) { 0, z }); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "init" "non-const" { target *-*-* } .-1 } */

/* Compound literals aren't themselves constant expressions.  */
int x = (int) { 0 }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "init" "non-const" { target *-*-* } .-1 } */

/* Nor are they suitable structure or union initializers
   outside a function.  */
struct s s1 = (struct s) { 0, 1 }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "init" "struct bad init" { target *-*-* } .-1 } */
union u u1 = (union u) { 0 }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "init" "union bad init" { target *-*-* } .-1 } */

/* They aren't suitable for array initializers, either inside or outside
   a function.  */
int y[2] = (int [2]) { 0, 1 }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "init" "array bad init" { target *-*-* } .-1 } */

void
bar (void)
{
  struct s s2 = (struct s) { 0, 1 };
  union u u2 = (union u) { 0 };
  int z[2] = (int [2]) { 0, 1 }; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "init" "array bad init" { target *-*-* } .-1 } */
}
