// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts-ts" }

template<typename T>
concept bool foo() { return true; };  // { dg-message "declared here" }

template<typename T>
void bar(T t)
{
  if constexpr (foo<T>::value)  // { dg-error "17:concept-id .foo<T>. in nested-name-specifier" }
  // { dg-error "expected|value" "" { target *-*-* } .-1 }
  {
  }
}

int main()
{
  bar(1);
}
