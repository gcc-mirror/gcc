// PR c++/32260
// { dg-do compile }
// { dg-options "-O2 -W -Wall" }

#include <typeinfo>

const std::type_info &
f1 (int i)
{
  return typeid (i + 1);
}

const std::type_info &
f2 ()
{
  return typeid (int);
}

struct A
{
  A ();
  virtual ~A ();
  void foo ();
};

const std::type_info &
f3 ()
{
  return typeid (A);
}

const std::type_info &
f4 (A *p)
{
  return typeid (*p);
}

const std::type_info &
f5 ()
{
  return typeid (int *);
}

const std::type_info &
f6 ()
{
  return typeid (int [26][12]);
}

const std::type_info &
f7 ()
{
  return typeid (int [26][12]);
}

void (A::*pmr) ();
const std::type_info &
f8 ()
{
  return typeid (pmr);
}
