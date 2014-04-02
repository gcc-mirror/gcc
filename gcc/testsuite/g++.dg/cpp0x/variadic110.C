// PR c++/45698
// { dg-do compile { target c++11 } }

template <class... Ts> struct tuple { };

template<class... Ts>
struct A {
  template<typename T> struct N { };
  tuple<N<Ts>...> tup;
};

int main()
{
  A<int, double> a;
}
