// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
concept foo = true;  // { dg-message "declared here" }

template<typename T>
void bar(T t)
{
  if constexpr (foo<T>::value)  // { dg-error "17:concept-id .foo<T>. in nested-name-specifier" }
  // { dg-error "expected|value" "" { target c++17 } .-1 }
  {
  }
}

int main()
{
  bar(1);
}
