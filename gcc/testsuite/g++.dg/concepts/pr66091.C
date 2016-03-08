// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
concept bool C1()
{
  return requires() { typename T::type1; };
}

template<typename T>
concept bool C2()
{
  return C1<T>() && requires() { typename T::type2; };
}

template<C1 T>
struct S {
  S& operator++() { return *this; }
  S& operator++() requires C2<T>() { return *this; }
};
