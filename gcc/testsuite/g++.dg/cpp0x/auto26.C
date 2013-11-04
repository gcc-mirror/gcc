// PR c++/43321
// { dg-options -std=c++11 }

template <class T>
void f(T t)
{
  auto *p = t;
}

template <class T>
void g(const T& tr)
{
  auto p = *tr;
}

int main()
{
  int b;
  f(&b);
  g(&b);
}
