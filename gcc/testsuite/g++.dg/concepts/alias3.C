// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  requires C<T>()
    using X = T*;

int main()
{
  X<int> x1; // { dg-error "constraint|invalid" }
}
