// generic error tests for generalized contract redecls
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

// allowed to repeat contracts or omit them
int g0(int a) [[ pre: a > 0 ]];
int g0(int a) [[ pre: a > 0 ]];

int g1(int a) [[ pre: a > 0 ]];
int g1(int a);

// allowed to add from none if generalized redecl is on (by default)
int g2(int a);
int g2(int a) [[ pre: a > 0 ]];

// can add to non-virtual methods
struct G0
{
  int f(int a);
};

int G0::f(int a) [[ pre: a > 0 ]]
{
  return -a;
}

struct G1
{
  int f(int a);
};

int G1::f(int a) [[ pre: a > 0 ]];

int G1::f(int a)
{
  return -a;
}

// allowed to redeclare even without contracts
struct G2
{
  int f(int a);
};

int G2::f(int a);


int f0(int a) [[ pre: a > 0 ]];
int f0(int a) [[ pre: a > 0 ]] [[ pre: a > 10 ]]; // { dg-error "different number of contracts" }

int f1(int a) [[ pre: a > 0 ]];
int f1(int a) [[ pre: a < 0 ]]; // { dg-error "mismatched contract" }

int f2(int a) { return a; }
int f2(int a) [[ pre: a < 0 ]]; // { dg-error "cannot add contracts after definition" }

struct Base
{
  virtual int f(int a) [[ pre: a > 0 ]];
};

struct Child : Base
{
  int f(int a) [[ pre: a < 0 ]]; // { dg-error "mismatched contract" }
};

// the initial decl of a guarded member must appear inside the class
struct F2
{
  int f(int a);
};

int F2::g(int a) [[ pre: a > 0 ]]; // { dg-error "no declaration matches" }
// FIXME if we move F2 down then a different error makes F2 undeclared

struct F0
{
  virtual int f(int a);
};

int F0::f(int a); // { dg-error "declaration.*is not definition" }

struct F1
{
  virtual int f(int a);
};

int F1::f(int a) [[ pre: a > 0 ]] // { dg-error "cannot add" }
{
  return -a;
}

// cannot "re"declare members of a forward declared class
struct F2;
int F2::test(); // { dg-error "no declaration matches" }
int F2::test2() [[ pre: true ]]; // { dg-error "no declaration matches" }

// can only redeclare member functions
struct F3
{
  int x;
  typedef int my_int;

  struct Inner0;
  struct Inner1;
  enum my_enum0; // { dg-error "use of enum.*without previous decl" }
  enum my_enum1 { E1, E2 };

  int test0();
  int test1();
  int test2();
};

int F3::x{-1}; // { dg-error "is not a static data member" }
typedef double F3::my_int; // { dg-error "typedef name may not be a nested-name-specifier" }
struct F3::Inner0; // { dg-warning "declaration.*does not declare anything" }

struct F3::Inner1 { };

enum F3::my_enum1 { E0, E1, END }; // { dg-error "multiple definition" }

struct F4
{
  int test0();

  int F3::test0() [[ pre: true ]]; // { dg-error "cannot declare member function" }
  friend int F3::test1();
  friend int F3::test2();
};
int F3::test2() [[ pre: true ]] { return -1; }

void dummy0()
{
  int F4::test0() [[ pre: true ]]; // { dg-error "qualified-id in declaration" }
}

namespace ns0
{
  typedef int value;
  struct X
  {
    int test1(value);
    typedef double value;
    int test2(value);
  };
  int X::test1(value); // { dg-error "no declaration matches" }
  int X::test2(value);
}

