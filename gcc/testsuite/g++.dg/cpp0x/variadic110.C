// PR c++/45698
// { dg-options -std=c++0x }

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
