// PR ipa/69239
// { dg-do run }
// { dg-options "-O2 --param=early-inlining-insns=196" }
// { dg-additional-options "-fPIC" { target fpic } }

struct D
{
  float f;
  D () {}
  virtual float bar (float z);
};

struct A
{
  A ();
  virtual int foo (int i);
};

struct B : public D, public A
{
  virtual int foo (int i);
};

float
D::bar (float)
{
  return f / 2;
}

int
A::foo (int i)
{
  return i + 1;
}

int
B::foo (int i)
{
  return i + 2;
}

int __attribute__ ((noinline,noclone))
baz ()
{
  return 1;
}

static int __attribute__ ((noinline))
fn (A *obj, int i)
{
  return obj->foo (i);
}

inline __attribute__ ((always_inline))
A::A ()
{
  if (fn (this, baz ()) != 2)
    __builtin_abort ();
}

static void
bah ()
{
  B b;
}

int
main ()
{
  bah ();
}
