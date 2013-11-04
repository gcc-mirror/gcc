// Test for narrowing diagnostics
// { dg-options "-std=c++11 -pedantic-errors" }

#include <initializer_list>

struct A { int i; int j; };
A a2 { 1.2 }; // { dg-error "narrowing" }
A a1 { 1, 2 }; // aggregate initialization 
struct B {
  B(std::initializer_list<int>);
};
B b1 { 1, 2 }; // creates initializer_list<int> and calls constructor
B b2 { 1, 2.0 }; // { dg-error "narrowing" }
struct C {
  C(int i, double j);
};
C c1 = { 1, 2.2 }; // calls constructor with arguments (1, 2.2) 
C c2 = { 1.1, 2 }; // { dg-error "narrowing" }

int j { 1 }; // initialize to 1
int k {}; // initialize to 0

// PR c++/36963
double d = 1.1;
float fa[] = { d, 1.1 };      // { dg-error "narrowing conversion of 'd'" }
constexpr double d2 = 1.1;
float fa2[] = { d2, 1.1 };

// PR c++/49577
unsigned u{ -1 };		// { dg-error "narrowing" }
char c = char{ u };		// { dg-error "narrowing" }

// PR c++/50011
short unsigned su;
int i { su };
