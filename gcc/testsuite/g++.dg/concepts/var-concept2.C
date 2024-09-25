// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept C1 = __is_class(T);

template<typename U>
  requires C1<U>
  void f1(U, U) { }

void f2(C1 auto) {}

int main ()
{
  f1(0, 0); // { dg-error "" }
  f2(1); // { dg-error "" }
  return 0;
}
