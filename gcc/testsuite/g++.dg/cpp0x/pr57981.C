// { dg-options "-std=c++11 -Wall -Wextra" }

template<class T>
void f(T t, void* = 0)  // { dg-warning "unused parameter" }
{
}

template<class T>
auto g(T t) -> decltype(f(t))
{
  f(t);
}

int main()
{
  g(0);
}
