// PR c++/5636
// Bug: the named return value optimization interfered with EH cleanups.

int c, d;

struct A
{
  A() { ++c; }
  ~A() { ++d; }
};

A f()
{
  A nrv;
  throw 42;
  return nrv;
}

int main()
{
  try
    { A a = f(); }
  catch (...) { }
  return (d < c);
}
