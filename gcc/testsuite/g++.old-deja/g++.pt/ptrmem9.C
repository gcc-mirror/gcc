// { dg-do assemble  }
// Origin: Jason Merrill <jason@cygnus.com>

struct A;
template <class T> void f (void (A::* const)(T)) {}
void (*p)(void (A::* const)(int)) = f;
