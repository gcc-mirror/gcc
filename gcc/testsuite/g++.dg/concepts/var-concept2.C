// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
  concept bool C1 = __is_class(T);

template<typename U>
  requires C1<U>
  void f1(U, U) { }

void f2(C1) {}

int main ()
{
  f1(0, 0); // { dg-error "cannot call" }
  f2(1); // { dg-error "cannot call" }
  return 0;
}
