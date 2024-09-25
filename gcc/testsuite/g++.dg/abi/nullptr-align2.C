// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=0 -Wabi=8" }

struct A
{
  decltype(nullptr) n;
  decltype(nullptr) n2;
};

struct B
{
  void *p;
  decltype(nullptr) n;
};

struct C
{
  char c;
  decltype(nullptr) n;		// { dg-warning "alignment" "" { target { ! default_packed } } }
};
