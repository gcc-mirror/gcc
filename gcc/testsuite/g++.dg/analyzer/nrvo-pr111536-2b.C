// { dg-do compile { target c++11 } }

struct g
{
  int t;
  g();
  g(const g&);
};

g foo1()
{
  return g{}; // { dg-bogus "uninitialized" }
}
