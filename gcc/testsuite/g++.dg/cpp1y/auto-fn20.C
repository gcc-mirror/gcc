// { dg-options "-std=c++1y" }

template <class T>
auto f(T) { return 42; }
template <class T>
auto g(T) { return 0.0; }

int main()
{
  int (*p)(int) = &f; // OK
  p = &g; // { dg-error "no match" }
}
