// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<typename T>
  concept bool C = __is_class(T);

C{T} void f1();

struct S1
{
  C{T} void f2();
  C{T} static void f3();
};

int main()
{
  S1 s;

  f1<S1>();
  s.f2<S1>();
  S1::f3<S1>();

  return 0;
}

template<typename T>
  void f1() requires C<T>
  {
  }

template<typename T>
  void S1::f2() requires C<T>
  {
  }

template<typename T>
  void S1::f3() requires C<T>
  {
  }
