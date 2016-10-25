// { dg-do run { target c++11 } }
// { dg-additional-options "-O2" }

void *
operator new (decltype (sizeof (0)), void *p)
{
  return p;
}

namespace std
{
  template <typename T>
  T *
  launder (T *p)
  {
    return __builtin_launder (p);
  }
}

struct A
{
  virtual int f ();
};

struct B : A
{
  virtual int f ()
  {
    new (this) A;
    return 1;
  }
};

int
A::f ()
{
  new (this) B;
  return 2;
}

static_assert (sizeof (B) == sizeof (A), "");

int
main ()
{
  A a;
  int n = a.f ();
  int m = std::launder (&a)->f ();
  if (n != 2 || m != 1)
    __builtin_abort ();
}
