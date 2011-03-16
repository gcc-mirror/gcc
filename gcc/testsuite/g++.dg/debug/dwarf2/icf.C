// { dg-do compile }
// { dg-options "-O1 -gdwarf-2 -gno-strict-dwarf -fno-inline -dA" }

class A
{
 public:
  A();
  virtual void work();
  virtual int p();
 private:
  int i;
};

class B
{
 public:
  B();
  ~B();
  void work(const A* a);
 private:
  int j;
};

int
test1(A* a)
{
  a->work();
}

int
test2(A* a)
{
  if (a->p())
    {
      B b;
      b.work(a);
    }
}
