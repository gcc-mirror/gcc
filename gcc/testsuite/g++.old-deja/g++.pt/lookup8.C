// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
class X{};

class Y : public X<int>
{
  void f();
};

void Y::f()
{
  X x; // ERROR - X is not a type.
}
