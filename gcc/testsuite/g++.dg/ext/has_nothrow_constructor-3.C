// PR c++/89914

struct A
{
  int i = ;  // { dg-error "expected" }
  // { dg-error "non-static data member" "" { target c++98_only } .-1 }
};

bool b = __has_nothrow_constructor (A);
