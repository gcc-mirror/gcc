// { dg-options "-std=gnu++11" }

#include <utility>

class A
{
public:

  A(int a, int& b, int&& c)
  : m_a{a},
    m_b{b},
    m_c{std::move(c)}
  {}

private:

  int m_a;
  int& m_b;
  int&& m_c;
};


struct X {};

class B
{
public:

  B(X& q, X&& r, const X& s)
  : m_q{q},
    m_r{std::move(r)},
    m_s{s}
  {}

private:

  X& m_q;
  X&& m_r;
  const X& m_s;
};
