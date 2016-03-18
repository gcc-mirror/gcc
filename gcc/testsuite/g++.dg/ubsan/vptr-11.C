// PR c++/70147
// { dg-do run }
// { dg-options "-fsanitize=vptr -fno-sanitize-recover=vptr" }

static int ac, ad, bc, bd, cc, cd, dc, dd;
struct A
{
  A ()
  {
    ac++;
  }
  virtual void f ()
  {
  }
  __attribute__ ((noinline)) ~ A ();
};

struct D
{
  __attribute__ ((noinline)) D (int);
  ~D ()
  {
    dd++;
  }
};
struct B: virtual A, D
{
  B ():D (1)
  {
    bc++;
  }
  virtual void f ()
  {
  }
  ~B ()
  {
    bd++;
  }
};

struct C: B, virtual A
{
  C ()
  {
    cc++;
  }
  ~C ()
  {
    cd++;
  }
};

D::D (int x)
{
  if (x)
    throw 1;
  dc++;
}

__attribute__ ((noinline, noclone))
void foo (A * p)
{
  p->f ();
}

A::~A ()
{
  foo (this);
  ad++;
}

int
main ()
{
  try
    {
      C c;
    }
  catch ( ...)
    {
    }
  if (ac != 1 || ad != 1 || bc || bd || cc || cd || dc || dd)
    __builtin_abort ();
}
