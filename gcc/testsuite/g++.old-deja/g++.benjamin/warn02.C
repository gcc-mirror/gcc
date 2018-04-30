// { dg-do assemble  }
// { dg-options "-Wredundant-decls" }
// 980413 bkoz 
// from g++/15307, tests for -Wredundant-decls 
// for friend functions and functions 


extern int foo(const char *);

class A
{
  friend int foo(const char *);
  int a;
};

class B
{
  friend int foo(const char *);
  int foo2() {return b;}
  int b;
};

class C
{
  friend int foo(const char *);
  friend int foo(const char *); // { dg-warning "" } 
  int foo2() {return b;}
  int b;
};

class D
{
public:
  int foo2() {return b;}  // { dg-message "previous" } 
  int foo2() {return b;}  // { dg-error "overloaded" } 
  int b;
};

class E
{
public:
  int foo2(); // { dg-message "previous" } 
  int foo2(); // { dg-error "overloaded" } 
  int b;
};

extern int foo3(const char *);  // { dg-message "" } 
extern int foo3(const char *);  // { dg-warning "" } 
