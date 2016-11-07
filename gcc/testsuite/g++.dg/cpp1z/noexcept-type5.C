// Test for composite pointer type.
// { dg-options -std=c++1z }

typedef void (*P)();
typedef void (*NP)() noexcept;

void f();
void g() noexcept;

bool b;

template <class T, class U> struct Same;
template <class T> struct Same<T,T> { };

Same<decltype(b ? &f : &g),P> s;

int main()
{
  P p = 0;
  NP np = 0;

  p == np;
  p != np;
  p < np;
}
