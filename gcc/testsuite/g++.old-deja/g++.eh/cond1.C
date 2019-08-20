// { dg-do assemble  }

// Copyright (C) 1999, 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 11 Apr 1999 <nathan@acm.org>
// Derived from bug report from Gabriel Dos Reis
// <Gabriel.Dos-Reis@cmla.ens-cachan.fr>
// http://gcc.gnu.org/ml/gcc-bugs/1999-03n/msg00888.html

// conditional exprs have some funny rules when one of the types is void.
// [expr.cond] 5.16, make sure we do the right things
// We implement an extension, allowing one side to be void, check we
// pedantically moan.

struct X {};
void fn(int i)
{
  int j;

  j = (i ? throw X() : 1); // ok, int
  j = (i ? 1 : throw X()); // ok, int
  
  (i ? throw X() : throw X());  // ok, void
  
  (i ? i : j) = 1; // ok, int &
  (i ? throw X() : j) = 1; // ok, int &
  (i ? j : throw X()) = 1; // ok, int &
  (i ? throw X() : throw X()) = 1;  // { dg-error "" } void
  
  (i ? (void)1 : i++); // { dg-error "" } ANSI forbids
  (i ? i++ : (void)1); // { dg-error "" } ANSI forbids
}
