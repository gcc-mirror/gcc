// 980413 bkoz 
// from g++/15307, tests for -Wredundant-decls 
// for friend functions and functions 
// Build don't link: 
//Special g++ Options: -Wredundant-decls


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
  friend int foo(const char *); // WARNING - 
  int foo2() {return b;}
  int b;
};

class D
{
public:
  int foo2() {return b;}  
  int foo2() {return b;}  // ERROR - 
  int b;
};

class E
{
public:
  int foo2(); 
  int foo2(); // ERROR - 
  int b;
};

extern int foo3(const char *);  // WARNING - 
extern int foo3(const char *);  // WARNING - 






