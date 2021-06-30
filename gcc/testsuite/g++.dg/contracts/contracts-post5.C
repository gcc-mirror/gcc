// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

// Tests for function templates

template<typename T>
T f1(T n)
  [[pre: n >= 0]]
  [[post r: r >= 0]]
  [[post r: !(r < 0)]]
{
  return n;
}


void driver()
{
  f1(0);
}