struct g
{
  int t;
  g();
};

g foo1()
{
  return g(); // { dg-bogus "uninitialized" }
}
