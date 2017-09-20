// { dg-options -std=c++17 }

template <int I, int J>
struct A { };

template <int I>
struct B
{
  template<int J>
  B(A<I,J>);
};

A<42,24> a;
B b (a);

int main()
{
  (B(a));
}
