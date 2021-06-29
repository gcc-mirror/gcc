// Test to ensure that diagnostic location for condition conversion is in the
// right place.
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }


template<typename T>
void fn()
  [[ pre: T{} ]] // { dg-error "no match" }
{
}

struct Z { };

int main(int, char**) {
  fn<int>();
  fn<Z>();
  return 0;
}
