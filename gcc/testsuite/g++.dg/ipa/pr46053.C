/* { dg-do run } */
/* { dg-options "-O -fipa-cp -fno-early-inlining"  } */

extern "C" void abort ();

struct A
{
  virtual void foo () = 0;
};

struct B : A
{
  virtual void foo () = 0;
};

struct C : A
{
};

struct D : C, B
{
  int i;
  D () : i(0xaaaa) {}
  virtual void foo ()
  {
    if (i != 0xaaaa)
      abort();
  }
};

static inline void bar (B &b)
{
  b.foo ();
}

int main()
{
  D d;
  bar (d);
  return 0;
}
