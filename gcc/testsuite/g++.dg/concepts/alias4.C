// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  requires C<T>()
    using X = T*;

// BUG: Alias templates are expanded at the point of use, regardless
// of whether or not they are dependent. This causes T* to be substituted
// without acutally checking the constraints.
template<typename T>
  using Y = X<T>;

int main()
{
  Y<int> y1; // { dg-error "" "" { xfail *-*-* } }
}
