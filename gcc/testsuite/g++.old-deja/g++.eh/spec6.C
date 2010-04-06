// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 19 Jan 1999 <nathan@acm.org>

// Determine that throw specifiers are checked correctly.

// [except.spec] 1, a type in an exception specifier shall not be incomplete,
// or pointer or ref to incomplete
struct X;                         // { dg-error "" } forward declaration.*
void fn1() throw(X);              // { dg-error "" } invalid use of undefined type
void fn2() throw(X *);            // { dg-error "" } invalid use of undefined type
void fn3() throw(X &);            // { dg-error "" } invalid use of undefined tyoe
void fn4() throw(void);           // { dg-error "" } invalid use of void expression
void fn5() throw(void &);         // { dg-error "" } invalid type // ERROR - invalid use of void
// except for cv pointer to void
void fn6() throw(void *);         // ok -- pointer to void
void fn7() throw(void const *);   // ok -- pointer to cv void

template<class T> void fny() throw(T);  // ok (so far)
template<> void fny<int>() throw(int);  // ok
template<> void fny<void>() throw(void); // { dg-error "" } invalid use of void

template<class T> void fnx(T *) throw(T){}  // { dg-error "" } invalid use of void expression
void fx()
{
  fnx((int *)0);
  fnx((void *)0);		// { dg-message "instantiated from here" }
}

// [except.spec] 2, exception specifiers must be the same set of types (but
// can be reordered)
void baz1() throw(int, char);
void baz1() throw(char, int){}       // reordering is ok

void baz2() throw(int, char);
void baz2() throw(int, char, int){}  // duplicates are ignored

typedef int Int;
void baz3() throw(int, char);
void baz3() throw(Int, char){}       // typedefs are the same type ...

void baz4() throw(int, Int, char);   // ... so this is a duplicate
void baz4() throw(Int, char){}

void fna() throw(int, char);  // { dg-error "" } to previous declaration
void fna() throw(int const, char);  // { dg-error "" } declaration  different exceptions // ERROR - to previous declaration
void fna() throw(int){}       // { dg-error "" } declaration  different exceptions

void fnb() throw(int, char);  // { dg-error "" } to previous declaration
void fnb() throw(char){}      // { dg-error "" } declaration  different exceptions

void fnc() throw(int, char);  // { dg-error "" } to previous declaration
void fnc() throw(char, int, float){}  // { dg-error "" } declaration  different exceptions

void fnd() throw();           // { dg-error "" } to previous declaration
void fnd() throw(char){}      // { dg-error "" } declaration  different exceptions

void fne() throw(char);       // { dg-error "" } to previous declaration
void fne() throw(){}          // { dg-error "" } declaration  different exceptions

void fnf();                   // { dg-error "" } to previous declaration
void fnf() throw(char){}      // { dg-error "" } declaration  different exceptions

void fng() throw(char);       // { dg-error "" } to previous declaration
void fng(){}                  // { dg-error "" } declaration  different exceptions

void fnh() throw(int, char);  // { dg-error "" } to previous declaration
void fnh() throw(int, float){}   // { dg-error "" } declaration  different exceptions

void fni() throw(int, char);  // { dg-error "" } to previous declaration
void fni() throw(float, char){}  // { dg-error "" } declaration  different exceptions

// [except.spec] 3, virtual function overriders shall throw a subset of the
// overridden function
struct E {};
struct F : public E {};
struct F1 : public E {};
struct G : public F, F1 {};
struct H : private E {};
struct A
{
  virtual void foo() throw();             // { dg-error "" } overriding 
  virtual void baz() throw(double, int);
  virtual void bar();
  virtual void qux() throw(E);
  virtual void qux(int) throw(E const *); // { dg-error "" } overriding (pedantically)
  virtual void quux() throw(F);           // { dg-error "" } overriding 
  virtual void quux(int) throw(F *);      // { dg-error "" } overriding 
  virtual void wibble() throw(E);         // { dg-error "" } overriding 
  virtual void wobble() throw(E *);       // { dg-error "" } overriding 
  virtual void wobble(int) throw(E *);    // { dg-error "" } overriding 
  virtual void wabble(int) throw(E *);
  virtual void wubble(int) throw(E *, H *);
  virtual ~A() throw();                   // { dg-error "" } overriding
};

struct B : A
{
  virtual void foo() throw(int);          // { dg-error "" } looser throw - A::foo
  virtual void baz() throw(double);       // ok subset
  virtual void bar(int) throw(int);       // ok not overriding
  virtual void qux() throw(F);            // ok subset
  virtual void qux(int) throw(F *);       // { dg-error "" } looser (pedantically)
  virtual void quux() throw(E);           // { dg-error "" } looser throw - A::quux()
  virtual void quux(int) throw(E *);      // { dg-error "" } looser throw - A::quux(int)
  virtual void wibble() throw(E *);       // { dg-error "" } looser throw - A::wibble
  virtual void wobble() throw(G *);       // { dg-error "" } looser throw - A::wobble()
  virtual void wobble(int) throw(H *);    // { dg-error "" } looser throw - A::wobble(int)
  virtual void wubble(int) throw(H *);    // ok
  virtual void wabble(int) throw(F1 *, F *);    // ok
};

struct A1
{
  virtual void foo() throw(int);
  virtual void bar() throw();       // { dg-error "" } overriding 
  virtual ~A1() throw(int);
};

struct B1 : A
{
};

struct C : A, A1		// { dg-error "" } looser throw - A::~A()
{
  virtual void foo() throw(int);    // { dg-error "" } looser throw - A::foo
  virtual void bar() throw(int);    // { dg-error "" } looser throw - A1::bar
};

struct D : A, A1
{
  virtual ~D() throw(int); // { dg-error "" } looser throw - A::~A()
};

// [except.spec] 5, types shall not be defined in exception specifiers
void fn8() throw(struct Z {}); // { dg-error "" } ANSI C++ forbids 
