// Build don't link:

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 19 Jan 1999 <nathan@acm.org>

// Determine that throw specifiers are checked correctly.

// [except.spec] 1, a type in an exception specifier shall not be incomplete,
// or pointer or ref to incomplete
struct X; // ERROR - forward declaration - XFAIL
void fn1() throw(X);  // ERROR - incomplete type - XFAIL
void fn2() throw(X *); // ERROR - incomplete type - XFAIL
void fn3() throw(X &); // ERROR - incomplete type - XFAIL
void fn4() throw(void); // ERROR - incomplete type - XFAIL
// except for cv pointer to void
void fn5() throw(void *);

// [except.spec] 2, exception specifiers must be the same set of types (but
// can be reordered)
void fn() throw(int, char);   // gets bogus error - XFAIL
void fn() throw(char, int){}  // gets bogus error - ordering is irrelevant - XFAIL

// [except.spec] 3, virtual function overriders shall throw a subset of the
// overridden function
struct E {};
struct F : public E {};
struct A
{
  virtual void foo() throw();
  virtual void baz() throw(double, int);
  virtual void bar();
  virtual void qux() throw(E);
  virtual void quux() throw(F);
};

struct B : A
{
  virtual void foo() throw(int);  // ERROR - not in base function - XFAIL
  virtual void baz() throw(double);
  virtual void bar(int) throw(int);
  virtual void qux() throw(F);
  virtual void quux() throw(E);   // ERROR - not in base function - XFAIL
};

// [except.spec] 5, types shall not be defined in exception specifiers
void fn6() throw(struct Z {}); // ERROR - types shall not be defined - XFAIL
