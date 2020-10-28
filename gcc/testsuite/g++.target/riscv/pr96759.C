/* { dg-options "-mno-strict-align -std=gnu++17" } */
/* { dg-do compile } */
struct S {
  int a;
  double b;
};
S GetNumbers();
auto [globalC, globalD] = GetNumbers();
