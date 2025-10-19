// test that attribute syntax is no longer recognized
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fcontracts" }

int fun(int n)  [[ pre : n > 0 ]]; // { dg-error {expected ']' before ':' token} }
// { dg-warning {'pre' attribute directive ignored} "" { target *-*-* } .-1 }
int fun2(const int n)  [[ post : n > 0 ]]; // { dg-error {expected ']' before ':' token} }
// { dg-warning {'post' attribute directive ignored} "" { target *-*-* } .-1 }

int main()
{
  int x;

  [[assert: x >= 0]]; // { dg-error {expected ']' before ':' token} }
  // { dg-warning {attributes at the beginning of statement are ignored} "" { target *-*-* } .-1 }

  return 0;
}
