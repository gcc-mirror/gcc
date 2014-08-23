// { dg-do run { target c++14 } }

template<int A, int B>
  struct S1
  {
    static constexpr int a = A;
    static constexpr int b = B;
  };

template<typename T>
  constexpr int var = T::a + T::b;

int main ()
{
  int v = var<S1<199, 23>>/2;
  return !(
       var<S1<11, 100>> == v
    && var<S1<50, 120>> == var<S1<150, var<S1<10, 10>>>>
    && var<S1<53, 23>> != 222
  );
}
