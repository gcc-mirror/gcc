// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
class X{};

class Y : public X<int>
{
  void f();
};

void Y::f()
{
  X x; // { dg-error "" } X is not a type.
}
