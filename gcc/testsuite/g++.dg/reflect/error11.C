// PR c++/123871
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

template<class T>
consteval auto
test ()
{
  auto lambda = [](this auto){
      constexpr std::meta::info array[1] = {^^T};
      using X = [: array[100] :];  // { dg-error "splice argument must be" }
      sizeof(X);
  };
}

void
g ()
{
  test<int>();
}
