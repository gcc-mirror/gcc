// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  requires C<T>()
    using X = T*;

struct S { };

int main()
{
  X<S> x1;
}
