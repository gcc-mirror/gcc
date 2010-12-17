// PR c++/33841
// { dg-do compile }

template<int> struct A
{
  // multiple errors below: missing semicolon, no anonymous structs, etc.
  struct {} : 2;	// { dg-error "" }
};

template<int> struct B
{
  int a;
  // multiple errors below: missing semicolon, no anonymous structs, etc.
  struct {} : 2;	// { dg-error "" }
  int b;
};

struct C : A<0> {};
struct D : B<0> {};
