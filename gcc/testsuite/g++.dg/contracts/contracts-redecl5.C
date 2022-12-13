// generic error tests for generalized contract redecls
//   we also test for the warning diagnostic for strict redecl
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontract-strict-declarations=on" }

// allowed to repeat contracts or omit them
int g0(int a) [[ pre: a > 0 ]];
int g0(int a) [[ pre: a > 0 ]];

int g1(int a) [[ pre: a > 0 ]];
int g1(int a);

// allowed to add from none if generalized redecl is on (by default)
int g2(int a);
int g2(int a) [[ pre: a > 0 ]]; // { dg-warning "adds contracts" }
int g2(int a) [[ pre: a > 0 ]]; // { dg-bogus "adds contracts" }

// can add to non-virtual methods
struct G0
{
  int f(int a);
};

int G0::f(int a) [[ pre: a > 0 ]] // { dg-warning "adds contracts" }
{
  return -a;
}

struct G1
{
  int f(int a);
};

int G1::f(int a) [[ pre: a > 0 ]]; // { dg-warning "adds contracts" }
// { dg-warning "outside of class is not definition" "" { target *-*-* } .-1 }

int G1::f(int a);
// { dg-warning "outside of class is not definition" "" { target *-*-* } .-1 }

int G1::f(int a) [[ pre: a > 0 ]];
// { dg-warning "outside of class is not definition" "" { target *-*-* } .-1 }

int G1::f(int a)
{
  return -a;
}

// allowed to redeclare even without contracts
struct G2
{
  int f(int a);
};

int G2::f(int a); // { dg-warning "outside of class is not definition" }


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

