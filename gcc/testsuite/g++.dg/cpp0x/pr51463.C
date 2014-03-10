// PR c++/51463
// { dg-do compile { target c++11 } }

struct A
{
  static virtual int i = 0;	// { dg-error "both virtual and static|declared as" }
};
