// PR c++/61020
// { dg-options "-O2" }
// { dg-do run }

#include <typeinfo>

struct Base {
  virtual ~Base() { }
};

struct Derived : public Base {
};

int compare(const Base& base)
{
  return typeid(base) == typeid(typeid(Derived));
}

int main()
{
  Base base;
  Derived derived;

  if (compare(base)) return 1;
  if (compare(derived)) return 2;
  return 0;
}
