// generic error tests for contract redecls with generalized redecl
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

// OK if equivalent -- even through renames.
int g0(int a) [[ pre: a > 0 ]];
int g0(int a) [[ pre: a > 0 ]];

int g0b(int a) [[ pre: a > 0 ]];
int g0b(int b) [[ pre: b > 0 ]];
int g0b(int c) [[ pre: c > 0 ]]
{
  return 0;
}

// OK if specified before.
int g1(int a) [[ pre: a > 0 ]];
int g1(int a);

// OK if specified after.
int g2(int a);
int g2(int a) [[ pre: a > 0 ]];

int g2b(int a);
int g2b(int b) [[ pre: b > 0 ]];

// can add to non-virtual methods
struct G0
{
  int f(int a);
};

// OK to add contracts at the point of definition.
int G0::f(int a) [[ pre: a > 0 ]]
{
  return -a;
}

struct G1
{
  int f1(int a);
};

// OK to redeclare functions and add constraints...
int G1::f1(int a) [[ pre: a > 0 ]];

// ...and leave them off later.
int G1::f1(int a)
{
  return -a;
}

int f0(int a) [[ pre: a > 0 ]];
int f0(int a) [[ pre: a > 0 ]] [[ pre: a > 10 ]]; // { dg-error "different number of contracts" }

int f1(int a) [[ pre: a > 0 ]] [[ pre: a > 10 ]];
int f1(int a) [[ pre: a > 0 ]]; // { dg-error "different number of contracts" }

int f2(int a) [[ pre: a > 0 ]];
int f2(int a) [[ pre: a < 0 ]]; // { dg-error "mismatched contract" }

int f3(int a) { return a; }
int f3(int a) [[ pre: a < 0 ]]; // { dg-error "cannot add contracts" }

struct Base
{
  virtual int f(int a) [[ pre: a > 0 ]];
};

struct Child : Base
{
  int f(int a) [[ pre: a < 0 ]]; // { dg-error "mismatched contract" }
};

struct S1
{
  virtual int f(int a); // contracts are inherited at the point of declarations
};

int S1::f(int a) [[ pre: a > 0 ]] // { dg-error "cannot add" }
{
  return -a;
}

struct S2
{
  int f() { return 0; }
};

int S2::f(); // OK?


struct S3
{
  int f() { return 0; }
};

int S3::f() [[pre: true]]; // { dg-error "cannot add contracts" }


// The initial decl of a guarded member must appear inside the class.
struct S4
{
  int f(int a);
};

int S4::g(int a) [[ pre: a > 0 ]]; // { dg-error "no declaration matches" }


struct S5
{
  template<typename T>
  S5(T a);
};

template<typename T>
S5::S5(T a) [[ pre: a > 0 ]]
{
}

struct S6
{
  template<typename T>
  S6(T a);
};

template<typename T>
S6::S6(T a) [[ pre: a > 0 ]];

template<typename T>
S6::S6(T a)
{
}

int p0(int n)
  [[ post r: r > 0 && r == n ]]
  [[ post r: r > 1 && r == n ]]
  [[ post r: r > 2 && r == n ]]
  [[ post r: r > 3 && r == n ]];

int p0(int z)
  [[ post r: r > 0 && r == z ]]
  [[ post r1: r1 > 1 && r1 == z ]]
  [[ post r2: r2 > 2 && r2 == z ]]
  [[ post r3: r3 > 3 && r3 == z ]]
{
  return z;
}

