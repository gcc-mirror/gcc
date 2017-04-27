/* Test for diagnostics for constant overflow.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <limits.h>

enum e {
  E0 = INT_MAX,
  /* Unsigned overflow wraps around.  */
  E1 = UINT_MAX + 1,
  /* Overflow in an unevaluated part of an expression is OK (example
     in the standard).  */
  E2 = 2 || 1 / 0,
  E3 = 1 / 0, /* { dg-warning "10:division by zero" } */
  /* { dg-error "3:enumerator value for 'E3' is not an integer constant" "enum error" { target *-*-* } .-1 } */
  /* But as in DR#031, the 1/0 in an evaluated subexpression means the
     whole expression violates the constraints.  */
  E4 = 0 * (1 / 0), /* { dg-warning "15:division by zero" } */
  /* { dg-error "enumerator value for 'E4' is not an integer constant" "enum error" { target *-*-* } .-1 } */
  E5 = INT_MAX + 1, /* { dg-warning "16:integer overflow in expression" } */
  /* Again, overflow in evaluated subexpression.  */
  E6 = 0 * (INT_MAX + 1), /* { dg-warning "21:integer overflow in expression" } */
  /* A cast does not constitute overflow in conversion.  */
  E7 = (char) INT_MAX
};

struct s {
  int a;
  int : 0 * (1 / 0); /* { dg-warning "16:division by zero" } */
  /* { dg-error "not an integer constant" "22:integer constant" { target *-*-* } .-1 } */
  int : 0 * (INT_MAX + 1); /* { dg-warning "integer overflow in expression" } */
};

void
f (void)
{
  /* This expression is not required to be a constant expression, so
     it should just involve undefined behavior at runtime.  */
  int c = INT_MAX + 1; /* { dg-warning "19:integer overflow in expression" } */
}

/* But this expression does need to be constant.  */
static int sc = INT_MAX + 1; /* { dg-warning "25:integer overflow in expression" } */

/* The first two of these involve overflow, so are not null pointer
   constants.  The third has the overflow in an unevaluated
   subexpression, so is a null pointer constant.  */
void *p = 0 * (INT_MAX + 1); /* { dg-warning "integer overflow in expression" } */
/* { dg-warning "initialization makes pointer from integer without a cast" "null" { target *-*-* } .-1 } */
void *q = 0 * (1 / 0); /* { dg-warning "division by zero" } */
/* { dg-error "initializer element is not computable at load time" "constant" { target *-*-* } .-1 } */
/* { dg-warning "initialization makes pointer from integer without a cast" "null" { target *-*-* } .-2 } */
void *r = (1 ? 0 : INT_MAX+1);

void
g (int i)
{
  switch (i)
    {
    case 0 * (1/0): /* { dg-warning "division by zero" } */
      /* { dg-error "case label does not reduce to an integer constant" "constant" { target *-*-* } .-1 } */
      ;
    case 1 + 0 * (INT_MAX + 1): /* { dg-warning "integer overflow in expression" } */
      ;
    }
}

int
h (void)
{
  return INT_MAX + 1; /* { dg-warning "integer overflow in expression" } */
}

int
h1 (void)
{
  return INT_MAX + 1 - INT_MAX; /* { dg-warning "integer overflow in expression" } */
}

void fuc (unsigned char);
void fsc (signed char);

void
h2 (void)
{
  fsc (SCHAR_MAX + 1);
  fsc (SCHAR_MIN - 1); /* { dg-warning "overflow in implicit constant conversion" } */
  fsc (UCHAR_MAX);
  fsc (UCHAR_MAX + 1); /* { dg-warning "overflow in implicit constant conversion" } */
  fuc (-1);
  fuc (UCHAR_MAX + 1); /* { dg-warning "large integer implicitly truncated to unsigned type" } */
  fuc (SCHAR_MIN);
  fuc (SCHAR_MIN - 1); /* { dg-warning "large integer implicitly truncated to unsigned type" } */
  fuc (-UCHAR_MAX); /* { dg-warning "large integer implicitly truncated to unsigned type" } */
}

void fui (unsigned int);
void fsi (signed int);

int si;
unsigned ui;

void
h2i (int x)
{
  /* For some reason, we only give certain warnings for implicit
     conversions among values of the same precision with -Wconversion,
     while we don't give others at all.  */
  fsi ((unsigned)INT_MAX + 1);
  si = (unsigned)INT_MAX + 1;
  si = x ? (unsigned)INT_MAX + 1 : 1;
  fsi ((unsigned)INT_MAX + 2);
  si = (unsigned)INT_MAX + 2;
  si = x ? (unsigned)INT_MAX + 2 : 1;
  fsi (UINT_MAX);
  si = UINT_MAX;
  fui (-1);
  ui = -1;
  ui = x ? -1 : 1U;
  fui (INT_MIN);
  ui = INT_MIN;
  ui = x ? INT_MIN : 1U;
}
