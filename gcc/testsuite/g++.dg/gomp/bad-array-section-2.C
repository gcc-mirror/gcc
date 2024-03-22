// { dg-do compile }
// { dg-additional-options "-std=c++11" }

template<typename T>
void foo()
{
  T arr[20];
  // Reject array section in lambda function.
#pragma omp target map([&](const int x) -> T* { return arr[0:x]; } (5))
// { dg-error {expected '\]' before ':' token} "" { target *-*-* } .-1 }
// { dg-error {invalid conversion from 'int' to 'int\*'} "" { target *-*-* } .-2 }
// { dg-error {expected ';' before ':' token} "" { target *-*-* } .-3 }
// { dg-error {expected primary-expression before ':' token} "" { target *-*-* } .-4 }
// { dg-message {sorry, unimplemented: unsupported map expression '<lambda closure object>foo<int>\(\)::<lambda\(int\)>\{arr\}.foo<int>\(\)::<lambda\(int\)>\(5\)'} "" { target *-*-* } .-5 }
  { }
}

int main()
{
  int arr[20];
  // Reject array section in lambda function.
#pragma omp target map([&](const int x) -> int* { return arr[0:x]; } (5))
// { dg-error {expected '\]' before ':' token} "" { target *-*-* } .-1 }
// { dg-error {invalid conversion from 'int' to 'int\*'} "" { target *-*-* } .-2 }
// { dg-error {expected ';' before ':' token} "" { target *-*-* } .-3 }
// { dg-error {expected primary-expression before ':' token} "" { target *-*-* } .-4 }
// { dg-message {sorry, unimplemented: unsupported map expression '<lambda closure object>main\(\)::<lambda\(int\)>\{arr\}.main\(\)::<lambda\(int\)>\(5\)'} "" { target *-*-* } .-5 }
  { }

  foo<int> ();

  return 0;
}
