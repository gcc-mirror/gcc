// { dg-do run  }
extern "C" void abort ();

struct A {
  unsigned long long u;
} *a;

struct B {
  unsigned long long v;
  unsigned long long w ()
  {
    return a->u - v;
  }
} b;

struct C {
  static unsigned long long x;
  static void y (unsigned long long z);
};

unsigned long long C::x = 0;

int main ()
{
  a = new A;
  b.v = 333418;
  a->u = 1132270;
  C::x = 0;
  C::y (799016);
}

void foo (unsigned long long a, unsigned long long b, unsigned long long c)
{
}

void C::y (unsigned long long z)
{
  unsigned long long c = b.w () - x;
  if (z < b.w ())
    {
      if ((long long) c < 0)
	{
	  foo (b.w (), a->u, b.v);
	  abort ();
	}
    }
}
