// DR 990
// { dg-options "-std=c++11" }

#include <initializer_list>

struct S {
  S(std::initializer_list<double>);  // #1
  S(std::initializer_list<int>);     // #2
  S();                                     // #3
  // ...
};
S s1 = { 1.0, 2.0, 3.0 };            // invoke #1
S s2 = { 1, 2, 3 };                  // invoke #2
S s3 = { };                          // invoke #3 (for value-initialization)


// Test some other situations, too.
void f (S);
int main()
{
  S s4 { };
  f({ });
  S {};
}
