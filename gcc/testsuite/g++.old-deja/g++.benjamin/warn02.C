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
  int foo2() {return b;}  
  int foo2() {return b;}  // { dg-error "" } 
  int b;
};

class E
{
public:
  int foo2(); 
  int foo2(); // { dg-error "" } 
  int b;
};

extern int foo3(const char *);  // { dg-warning "" } 
extern int foo3(const char *);  // { dg-warning "" } 






