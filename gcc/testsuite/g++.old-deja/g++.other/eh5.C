// { dg-do assemble  }
// { dg-options "-O2" }

struct X {};

struct Y
{
  Y();
  virtual ~Y();
};

struct Z
{
  int f (const int *x);
  int g;
};

inline int
operator<< (Z &os, int x)
{
  os.f (&x);
  return os.g;
}

void foo (Z &a, X *b, X &c)
{
  X *d = b;
  int e = 0;
  Z &f = a;
  if (!(f << e))
    do { do { } while (&c == 0); throw Y(); } while (0);
  do { } while (&d == 0);
  do { } while (&c == 0);
}
