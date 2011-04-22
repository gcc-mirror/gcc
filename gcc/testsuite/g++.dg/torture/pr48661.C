// PR middle-end/48661
// { dg-do run }

extern "C" void abort ();

__attribute__((noinline))
double
foo (double x, double y)
{
  asm volatile ("" : : : "memory");
  return x + y;
}

__attribute__((noinline, noclone))
void
bar (int x)
{
  if (x != 123)
    abort ();
}

struct A
{
  double a1, a2;
};

struct B 
{
  virtual int m () const = 0 ;
};

struct C
{
  virtual ~C () {}
};

struct D : virtual public B, public C
{ 
  explicit D (const A &x) : d(123) { foo (x.a2, x.a1); }
  int m () const { return d; }
  int d;
}; 

struct E
{
  E () : d(0) {}
  virtual void n (const B &x) { d = x.m (); x.m (); x.m (); }
  int d;
};

void
test ()
{
  A a;
  a.a1 = 0;
  a.a2 = 1;
  E p;
  D q (a);
  const B &b = q;
  bar (b.m ());
  p.n (b);
  bar (p.d);
}

void
baz ()
{
  A a;
  D p2 (a);
}

int
main ()
{
  test ();
  return 0;
}
