// { dg-do compile }
// Contributed by Wolfgang Wieser <wwieser at gmx dot de>
// PR c++/15967: ICE with ambiguous operator new

typedef __SIZE_TYPE__ size_t;

struct A { void *operator new(size_t s){} };
struct B { void *operator new(size_t s){} };

struct C : A,B {};

int crash()
{
  C *c=new C();   // { dg-error "ambiguous" }
}
