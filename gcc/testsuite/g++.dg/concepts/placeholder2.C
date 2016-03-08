// { dg-options "-std=c++1z -fconcepts" }

// Check argument deduction constraints.
// TODO: We shoul have more of these...

template<typename T>
concept bool C1 = sizeof(T) == 0;

template<typename T, typename U>
concept bool C2 = __is_same_as(T, U);


template<typename T>
concept bool D1()
{
  return requires (T t) { { t } -> C1; };
}

template<typename T>
concept bool D2()
{
  return requires (T t) { { t } -> C2<void>; };
}

void f1(D1) { }
void f2(D2) { }

int main()
{
  f1(0); // { dg-error "cannot call" }
  f2(0); // { dg-error "cannot call" }
}
