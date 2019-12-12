// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<typename ... T>
  concept bool C1 = true;

template<int ... N>
  concept bool C2 = true;

template<typename T>
  concept bool C3 = __is_class(T);

template<typename ... T>
  concept bool C4() { return true; }
template<int N>
  concept bool C4() { return true; }

template<typename T, typename U = int>
  concept bool C5() { return __is_class(U); }

C1{...A, B} void f1() {}; // { dg-error "cannot deduce template parameters" }
C1{A} void f2() {}
C2{A, B} void f3() {};
C3{...A} void f4() {}; // { dg-error "cannot be introduced" }
C4{A} void f5() {}; // { dg-error "cannot deduce template parameters" }
C5{A, B} void f6() {};

int main()
{
  // Defaults should not transfer
  f6<int>(); // { dg-error "no matching" }
  return 0;
}
