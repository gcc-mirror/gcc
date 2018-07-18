// PR c++/79363 - ICE with NSDMI and array
// { dg-do compile { target c++11 } }
// { dg-options -Wno-pedantic }

struct A
{
  int i;
  int a[] = { };   // { dg-error "initializer for flexible array member" }
} a;

struct B
{
  int i;
  char a[] { "abc" };   // { dg-error "initializer for flexible array member" }
} b;

struct C
{
  int i;
  char a[];
  C (): a ("def") { }   // { dg-error "initializer for flexible array member" }
} c;

struct D
{
  struct X { };
  int i;
  X x[] = { };   // { dg-error "initializer for flexible array member" }
} d;
