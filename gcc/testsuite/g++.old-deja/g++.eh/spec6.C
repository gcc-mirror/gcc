// Build don't link:

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 19 Jan 1999 <nathan@acm.org>

// Determine that throw specifiers are checked correctly.

// [except.spec] 1, a type in an exception specifier shall not be incomplete,
// or pointer or ref to incomplete
struct X;                         // ERROR - forward declaration.*
void fn1() throw(X);              // ERROR - invalid use of undefined type
void fn2() throw(X *);            // ERROR - invalid use of undefined type
void fn3() throw(X &);            // ERROR - invalid use of undefined tyoe
void fn4() throw(void);           // ERROR - invalid use of void expression
void fn5() throw(void &);         // ERROR - invalid type // ERROR - invalid use of void
// except for cv pointer to void
void fn6() throw(void *);         // ok -- pointer to void
void fn7() throw(void const *);   // ok -- pointer to cv void

template<class T> void fny() throw(T);  // ok (so far)
template<> void fny<int>() throw(int);  // ok
template<> void fny<void>() throw(void); // ERROR - invalid use of void

template<class T> void fnx(T *) throw(T){}  // ERROR - invalid use of void expression
void fx()
{
  fnx((int *)0);
  fnx((void *)0);		// ERROR - instantiated from here
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

void fna() throw(int, char);  // ERROR - to previous declaration
void fna() throw(int const, char);  // ERROR - declaration  different exceptions // ERROR - to previous declaration
void fna() throw(int){}       // ERROR - declaration  different exceptions

void fnb() throw(int, char);  // ERROR - to previous declaration
void fnb() throw(char){}      // ERROR - declaration  different exceptions

void fnc() throw(int, char);  // ERROR - to previous declaration
void fnc() throw(char, int, float){}  // ERROR - declaration  different exceptions

void fnd() throw();           // ERROR - to previous declaration
void fnd() throw(char){}      // ERROR - declaration  different exceptions

void fne() throw(char);       // ERROR - to previous declaration
void fne() throw(){}          // ERROR - declaration  different exceptions

void fnf();                   // ERROR - to previous declaration
void fnf() throw(char){}      // ERROR - declaration  different exceptions

void fng() throw(char);       // ERROR - to previous declaration
void fng(){}                  // ERROR - declaration  different exceptions

void fnh() throw(int, char);  // ERROR - to previous declaration
void fnh() throw(int, float){}   // ERROR - declaration  different exceptions

void fni() throw(int, char);  // ERROR - to previous declaration
void fni() throw(float, char){}  // ERROR - declaration  different exceptions

// [except.spec] 3, virtual function overriders shall throw a subset of the
// overridden function
struct E {};
struct F : public E {};
struct F1 : public E {};
struct G : public F, F1 {};
struct H : private E {};
struct A
{
  virtual void foo() throw();             // ERROR - overriding 
  virtual void baz() throw(double, int);
  virtual void bar();
  virtual void qux() throw(E);
  virtual void qux(int) throw(E const *); // ERROR - overriding (pedantically)
  virtual void quux() throw(F);           // ERROR - overriding 
  virtual void quux(int) throw(F *);      // ERROR - overriding 
  virtual void wibble() throw(E);         // ERROR - overriding 
  virtual void wobble() throw(E *);       // ERROR - overriding 
  virtual void wobble(int) throw(E *);    // ERROR - overriding 
  virtual void wabble(int) throw(E *);
  virtual void wubble(int) throw(E *, H *);
  virtual ~A() throw();                   // ERROR - overriding
};

struct B : A
{
  virtual void foo() throw(int);          // ERROR - looser throw - A::foo
  virtual void baz() throw(double);       // ok subset
  virtual void bar(int) throw(int);       // ok not overriding
  virtual void qux() throw(F);            // ok subset
  virtual void qux(int) throw(F *);       // ERROR - looser (pedantically)
  virtual void quux() throw(E);           // ERROR - looser throw - A::quux()
  virtual void quux(int) throw(E *);      // ERROR - looser throw - A::quux(int)
  virtual void wibble() throw(E *);       // ERROR - looser throw - A::wibble
  virtual void wobble() throw(G *);       // ERROR - looser throw - A::wobble()
  virtual void wobble(int) throw(H *);    // ERROR - looser throw - A::wobble(int)
  virtual void wubble(int) throw(H *);    // ok
  virtual void wabble(int) throw(F1 *, F *);    // ok
};

struct A1
{
  virtual void foo() throw(int);
  virtual void bar() throw();       // ERROR - overriding 
  virtual ~A1() throw(int);
};

struct B1 : A
{
};

struct C : A, A1
{ // ERROR - looser throw - A::~A()
  virtual void foo() throw(int);    // ERROR - looser throw - A::foo
  virtual void bar() throw(int);    // ERROR - looser throw - A1::bar
};

struct D : A, A1
{
  virtual ~D() throw(int); // ERROR - looser throw - A::~A()
};

// [except.spec] 5, types shall not be defined in exception specifiers
void fn8() throw(struct Z {}); // ERROR - ANSI C++ forbids 
