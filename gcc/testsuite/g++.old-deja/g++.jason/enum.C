// { dg-do assemble  }
// PRMS Id: 4337
// Bug: Enums are not looked up to arbitrary depth.

struct W {
  enum A { B };
};

struct X : public W
{};

struct Y : public X
{};

struct S
{
  X::A a1;
  Y::A a2;			// { dg-bogus "" } 
};
