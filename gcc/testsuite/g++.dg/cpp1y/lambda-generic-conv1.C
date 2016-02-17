// PR c++/69842
// { dg-do compile { target c++14 } }

template <class T, class U> struct same;
template <class T> struct same<T,T> {};

int main()
{
  auto g = [](auto && _var) {
    same<int&&,decltype(_var)>();
  };

  g(0);
}
