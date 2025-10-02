/* Test invalid initializers that are consistent with the syntax: undefined
   behavior ("shall" in Semantics not Constraints) before C2y, constraint
   violation in C2y.  Scalar cases; see bug 88642.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

struct s { int a; };
union u { int a; };

int i1 = { 1, 2 }; /* { dg-error "excess elements in scalar initializer" } */
int i2 = { { 1 } }; /* { dg-error "braces around scalar initializer" } */
int i3 = { { 1, } }; /* { dg-error "braces around scalar initializer" } */
int i4 = { { 1 }, }; /* { dg-error "braces around scalar initializer" } */
int i5 = { 1, { } }; /* { dg-error "excess elements in scalar initializer" } */
/* { dg-error "braces around scalar initializer" "braces" { target *-*-* } .-1 } */
int i6 = { { } }; /* { dg-error "braces around scalar initializer" } */
int i7 = { { }, }; /* { dg-error "braces around scalar initializer" } */
int i8 = { { { 1 } } }; /* { dg-error "braces around scalar initializer" } */
struct s s1 =
  {
    { /* { dg-warning "braces around scalar initializer" } */
      { 1 } /* { dg-error "braces around scalar initializer" } */
    }
  };
union u u1 =
  {
    { /* { dg-warning "braces around scalar initializer" } */
      { 1 } /* { dg-error "braces around scalar initializer" } */
    }
  };
int a1[1] =
  {
    { /* { dg-warning "braces around scalar initializer" } */
      { 1 } /* { dg-error "braces around scalar initializer" } */
    }
  };
int *p1 = &(int) { { 1 } }; /* { dg-error "braces around scalar initializer" } */
int *p2 = &(int) { { 1, } }; /* { dg-error "braces around scalar initializer" } */

int ok1 = { 1 };
struct s ok2 = { { 1 } }; /* { dg-warning "braces around scalar initializer" } */
struct s ok3 = { { 1, } }; /* { dg-warning "braces around scalar initializer" } */
int *ok4 = &(int) { 1 };
int *ok5 = &(int) { 1, };
int ok6[1] = { { 1 } }; /* { dg-warning "braces around scalar initializer" } */
int ok7[1] = { { 1, } }; /* { dg-warning "braces around scalar initializer" } */
union u ok8 = { { 1 } }; /* { dg-warning "braces around scalar initializer" } */
union u ok9 = { { 1, } }; /* { dg-warning "braces around scalar initializer" } */
