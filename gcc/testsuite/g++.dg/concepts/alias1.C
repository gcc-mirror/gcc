// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

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
