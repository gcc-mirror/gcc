// PR c++/19966
// { dg-do compile }

struct A
{
  static operator int(); // { dg-error "must be a nonstatic" }
};

struct B
{
  static int operator*(); // { dg-error "must be either" }
};

static operator int(); // { dg-error "must be a nonstatic" }
