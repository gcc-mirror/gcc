// { dg-do run  }
#include <typeinfo>

class Base
{
public:
  virtual ~Base() { }
};

class Derived : public Base
{
public:
    Derived() {}
};

int main()
{
    const Derived b;
    const Base*   ap = &b;

    const Derived* p1 = dynamic_cast<const Derived*>(ap);
    return p1 == 0;
}
