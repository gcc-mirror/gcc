// Test to ensure that diagnostic location for condition conversion is in the
// right place.
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

struct Z {};

template<typename T>
void fn(T t)
[[ pre: t ]] // { dg-error "could not convert" }
{
}


int main(int, char**) {
  fn(1);
  fn(Z{});
  return 0;
}
