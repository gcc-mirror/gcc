// Special Options: -Wconversion

extern "C" void abort();

struct A {
  typedef double (&B);
  typedef const double (&C);

  A() { }

  operator C () const;
  operator B ();
};

static const double d = 2.0;
static double e = 3.0;

A::operator A::C () const
{
  abort ();
  return d;
}

A::operator A::B ()
{
  return e;
}

int main ()
{
  (A::C) A ();		// WARNING - 
  return 0;
}
