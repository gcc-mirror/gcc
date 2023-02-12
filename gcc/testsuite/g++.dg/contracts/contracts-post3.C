// { dg-do run }
// { dg-options "-std=c++2a -fcontracts" }
// { dg-shouldfail "assert violation" }
// { dg-output "contract violation in function f1" }

int f1(int n)
  [[post r: r > n]]
{
  return n;
}

int main()
{
  f1(0);
}
