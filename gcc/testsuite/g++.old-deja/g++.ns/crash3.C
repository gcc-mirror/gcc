// Build don't link:

namespace N {
  template <class T> struct S;
};

void f()
{
  N::S(); // ERROR - invalid use of template
}
