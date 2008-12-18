// PR c++/38427
// { dg-do compile }

struct S
{
  int &ref;
  S() : ref() {};	// { dg-error "value-initialization of" }
};
