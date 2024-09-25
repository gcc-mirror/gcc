// { dg-do run }
/* { dg-options "-O2" } */
/* { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } } */

// We used to get alias grouping wrong on this one, hoisting accesses
// to the vector's end out of the loop.

#include <vector>
#include <cassert>

struct A
{
  double unused;      // If I remove it => it works.
  std::vector<int> v;

  A() : v(1) {}
};

inline // If not inline => it works.
A g()
{
  A r;
  r.v.resize(2);
  r.v[0] = 1;

  while (!r.v.empty() && r.v.back() == 0)
    r.v.pop_back();

  return r;
}

A f(const A &a)
{
  if (a.v.empty())  return a;
  if (a.v.empty())  return a;

  // A z = g(); return z;  // If I return like this => it works.
  return g();
}

int main()
{
  A a;
  A b;
  A r = f(a);
  assert(r.v.size() != 0);

  return 0;
}
