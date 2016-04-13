// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<C T> using X = T*;

struct S { };

int main()
{
  X<S> x1;
}
