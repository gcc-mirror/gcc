// { dg-do compile { target c++14 } }

template <class T>
auto f(T) { return 42; }
template <class T>
auto g(T) { return 0.0; }

int main()
{
  int (*p)(int) = &f; // OK
  p = &g; // { dg-error "no match" }
}
