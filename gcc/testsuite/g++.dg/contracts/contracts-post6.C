// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

// Test for class members

template<typename T>
struct S
{
  S(T n)
    [[post: true]]
    : n(n)
  { }

  T f1(T n)
    [[pre: n >= 0]]
    [[post r: r >= 0]]
    [[post r: !(r < 0)]]
  {
    return n;
  }

  T n;
};


void driver()
{
  S<int> s1(0);
  s1.f1(2);
}
