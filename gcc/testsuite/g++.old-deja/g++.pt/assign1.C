// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S {
  S();
  T t;
};

void f()
{
  S<const int> s;
  s = s; // ERROR - generated assignment operator is illegal
}
