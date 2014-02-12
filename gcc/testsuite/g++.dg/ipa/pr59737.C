// PR middle-end/59737
// { dg-do compile }
// { dg-options "-O2" }

struct A
{
  virtual void foo (int &x);
  friend void
  operator>> (int &x, A &y)
  {
    y.foo (x);
  }
};

struct B : public A
{
  void foo (int &x);
};

struct F : public B
{
  void foo (int &x);
};

struct G : public F
{
  void foo (int &);
};

struct C : A
{
  void foo (int &);
  struct H : public G
  {
    void foo (int &);
  };
  struct D : A
  {
    H d;
  };
};

void
C::foo (int &x)
{
  D a;
  x >> a.d;
}
