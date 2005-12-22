// PR optimization/4994
// This testcase ICEd because movsi was not supporting direct
// mmx -> mmx register moves.
// { dg-do compile }
// { dg-options "-O2" }
// { dg-options "-fno-exceptions -O2 -mmmx -fPIC" { target { { i?86-*-* x86_64-*-* } && ilp32 }  } }

struct A {
  unsigned a0;
  bool a1 () { return !--a0; }
  void a2 ();
};

struct B
{
  B ();
  B (const B &);
  ~B();
  B &operator= (const B &);
  B b0 (unsigned long x, int y = 0, int z = 10) const;

private:
  A *b1;
  static A *b2;
};

inline B::~B()
{
  if (b1->a1 () && b1 == b2)
    b1->a2();
}

struct C
{
  C *c0;
};

struct D
{
  C *d0;
  D ();
  D (const D &c0) {}
  D &operator++ () {
    C *x = d0; C *y = x->c0;
    while (x == y->c0)
      x = y;
    d0 = x;
    return *this;
  }
};

B foo (const char *x, const B &y);

void bar (void)
{
  B *y = 0;
  B z;
  for (unsigned long l = 0; l < 2147483647L * 2UL + 1; l++)
    {
      z = y->b0 (l);
      *y = foo ("data", z);
    }
  D d;
  ++d;
}
