/* Test for diagnostics for constant overflow.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-fpermissive" } */

#include <limits.h>

enum e {
  E0 = INT_MAX,
  /* Unsigned overflow wraps around.  */
  E1 = UINT_MAX + 1,
  /* Overflow in an unevaluated part of an expression is OK (example
     in the standard).  */
  E2 = 2 || 1 / 0, /* { dg-bogus "warning: division by zero" "" { xfail *-*-* } } */
  E3 = 1 / 0, /* { dg-warning "division by zero" } */
  /* { dg-error "enumerator value for 'E3' is not an integer constant|not a constant.expression" "enum error" { target *-*-* } .-1 } */
  /* But as in DR#031, the 1/0 in an evaluated subexpression means the
     whole expression violates the constraints.  */
  E4 = 0 * (1 / 0), /* { dg-warning "division by zero" } */
  /* { dg-error "enumerator value for 'E4' is not an integer constant" "enum error" { target c++ } .-1 } */
  /* { dg-error "division by zero is not a constant.expression" "division" { target c++11 } .-2 } */
  E5 = INT_MAX + 1, /* { dg-warning "integer overflow in expression" } */
  /* { dg-warning "overflow in constant expression" "constant" { target *-*-* } .-1 } */
  /* Again, overflow in evaluated subexpression.  */
  E6 = 0 * (INT_MAX + 1), /* { dg-warning "integer overflow in expression" } */
  /* { dg-warning "overflow in constant expression" "constant" { target *-*-* } .-1 } */
  /* A cast does not constitute overflow in conversion.  */
  E7 = (char) INT_MAX
};

struct s {
  int a;
  int : 0 * (1 / 0); /* { dg-warning "division by zero" } */
/* { dg-error "division by zero is not a constant.expression" "division" { target c++11 } .-1 } */
/* { dg-error "width not an integer constant" "bit.field" { target c++ } .-2 } */
/* { dg-error "is not a constant expression" "division" { target c++ } .-3 } */
  int : 0 * (INT_MAX + 1); /* { dg-warning "integer overflow in expression" } */
  /* { dg-warning "overflow in constant expression" "constant" { target *-*-* } .-1 } */
};

void
f (void)
{
  /* This expression is not required to be a constant expression, so
     it should just involve undefined behavior at runtime.  */
  int c = INT_MAX + 1; /* { dg-warning "integer overflow in expression" } */
}

/* This expression is neither required to be constant.  */
static int sc = INT_MAX + 1; /* { dg-warning "integer overflow in expression" } */


// Test for overflow in null pointer constant.  
void *n = 0;
/* The first two of these involve overflow, so are not null pointer
   constants.  The third has the overflow in an unevaluated
   subexpression, so is a null pointer constant.  */
void *p = 0 * (INT_MAX + 1); /* { dg-warning "integer overflow in expression" } */
/* { dg-warning "invalid conversion from 'int' to 'void" "null" { target *-*-* } .-1 } */

void *q = 0 * (1 / 0);
/* { dg-warning "division by zero" "" { target *-*-* } .-1 } */
/* { dg-error "invalid conversion from 'int' to 'void*'" "null" { xfail *-*-* } .-2 } */
/* { dg-warning "invalid conversion from" "convert" { target *-*-* } .-3 } */

void *r = (1 ? 0 : INT_MAX+1);
/* { dg-bogus "integer overflow in expression" "" { xfail *-*-* } .-1 } */
/* { dg-warning "invalid conversion from" "convert" { target c++11 } .-2 } */

void
g (int i)
{
  switch (i)
    {
    case 0 * (1/0):
      /* { dg-warning "division by zero" "" { target *-*-* } .-1 } */
      /* { dg-error "division by zero is not a constant expression" "division" { target c++11 } .-2 } */
      /* { dg-error "is not a constant expression" "const" { target *-*-* } .-3 } */
      ;
    case 1 + 0 * (INT_MAX + 1): /* { dg-warning "integer overflow in expression" } */
      /* { dg-warning "overflow in constant expression" "constant" { target *-*-* } .-1 } */
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
  fsc (SCHAR_MIN - 1); /* { dg-warning "overflow in conversion from .int. to .signed char. changes value" } */
  fsc (UCHAR_MAX);
  fsc (UCHAR_MAX + 1); /* { dg-warning "overflow in conversion from .int. to .signed char. changes value" } */
  fuc (-1);
  fuc (UCHAR_MAX + 1); /* { dg-warning "unsigned conversion from .int. to .unsigned char. changes value" } */
  fuc (SCHAR_MIN);
  fuc (SCHAR_MIN - 1); /* { dg-warning "unsigned conversion from .int. to .unsigned char. changes value" } */
  fuc (-UCHAR_MAX); /* { dg-warning "unsigned conversion from .int. to .unsigned char. changes value" } */
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
