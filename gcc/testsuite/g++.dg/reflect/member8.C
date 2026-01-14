// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct S {
  int k;
};

struct Y {
  int k;
};
struct X : S { };

class D : S , X {} d;	// { dg-warning "direct base .S. inaccessible in .D. due to ambiguity" }
int dK = d.[:^^S::k:];	// { dg-error ".S. is an ambiguous base of .D." }
int dY = d.[:^^Y::k:];	// { dg-error ".Y. is not a base of .D." }
