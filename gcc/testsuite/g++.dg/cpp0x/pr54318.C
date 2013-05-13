// PR c++/54318
// { dg-do compile { target c++11 } }

template <typename T>
struct wrapped
{
  typedef T type;
};

template <typename T>
typename T::type unwrap1(T);

int unwrap(int);

template <typename T>
auto unwrap(T t) -> decltype(unwrap(unwrap1(t)))
{
  return unwrap(unwrap1(t));
}

int main()
{
  unwrap(wrapped<wrapped<int>>());
}
