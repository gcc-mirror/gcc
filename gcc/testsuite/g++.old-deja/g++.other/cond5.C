// { dg-do assemble  }
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.
// { dg-options "-W -pedantic -ansi" }

// Copyright (C) 1999, 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 Sep 1999 <nathan@acm.org>
// Derived from bug report from Gabriel Dos Reis
// <Gabriel.Dos-Reis@cmla.ens-cachan.fr>
// http://gcc.gnu.org/ml/gcc-bugs/1999-03n/msg00888.html

// conditional exprs have some funny rules when one of the types is void.
// [expr.cond] 5.16, make sure we do the right things
// We have checks for mismatching enumerations, check we give them -- they had
// got lost due to changes in add_builtin_candidate and subsequent changes.

struct X {};
enum E1 {e1 = -1};
enum E2 {e2 = -1};
void f(int, ...);

void fn(int i)
{
  double d;
  int j;

  j = (i ? e1 : e2);    // { dg-warning "mismatch" }
  d = (i ? e1 : 1.0);   // { dg-warning "non-enumerated" }
  d = (i ? 1.0 : e2);   // { dg-warning "non-enumerated" }
  E1 e = (i ? e1 : e1); // ok
  j = (i ? 1 : e2);     // ok
  j = (i ? e1 : 1);     // ok
  
  j = (i ? throw X() : 1); // ok, int
  j = (i ? 1 : throw X()); // ok, int
  
  (i ? throw X() : throw X());  // ok, void
  
  (i ? i : j) = 1; // ok, int &
  (i ? throw X() : j) = 1; // ok, int &
  (i ? j : throw X()) = 1; // ok, int &
  (i ? throw X() : throw X()) = 1;  // { dg-error "lvalue" }
  
  (i ? (void)1 : i++);        // { dg-error "throw-expression" }
  (i ? i++ : (void)1);        // { dg-error "throw-expression" }
  
}
