// PR c++/69842
// { dg-do compile { target c++14 } }

template <class T, class U> struct assert_same;
template <class T> struct assert_same<T,T> {};

template<typename T>
void sink(T &&)
{
  assert_same<int,T> a;
}

int main()
{
  auto const g([](auto &&...  _var) {
      sink(static_cast<decltype(_var)>(_var)...);
    });

  g(0);
}
