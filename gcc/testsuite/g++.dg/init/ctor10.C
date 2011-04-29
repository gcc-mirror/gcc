// PR c++/48606
// { dg-do compile }
// { dg-options "-fkeep-inline-functions" }

struct S
{
  int &ref;
  S() : ref() {};	// { dg-error "value-initialization of" }
};
