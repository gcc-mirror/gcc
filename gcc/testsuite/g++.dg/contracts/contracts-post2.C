// { dg-do run }
// { dg-options "-std=c++2a -fcontracts" }
// { dg-skip-if "requires hosted libstdc++ for stdc++exp" { ! hostedlib } }

int f1(int n)
  [[post r: r == n]]
{
  return n;
}

int main()
{
  f1(0);
}
