// { dg-do run }
// { dg-options "-O3" }

struct Distraction
{
  char fc[8];
  virtual Distraction * return_self ()
  { return this; }
};

namespace {

struct A;
static A * __attribute__ ((noinline, noclone)) get_an_A ();

static int go;

struct A
{
  int fi;

  A () : fi(777) {}
  A (int pi) : fi (pi) {}
  virtual A * foo (int p) = 0;
};

struct B;
static B * __attribute__ ((noinline, noclone)) get_a_B ();

struct B : public Distraction, A
{
  B () : Distraction(), A() { }
  B (int pi) : Distraction (), A (pi) {}
  virtual B * foo (int p)
  {
    int o = fi;
    for (int i = 0; i < p; i++)
      o += i + i * i;
    go = o;

    return get_a_B ();
  }
};


struct B gb1 (1111), gb2 (2);
static B * __attribute__ ((noinline, noclone))
get_a_B ()
{
  return &gb1;
}

static A * __attribute__ ((noinline, noclone))
get_an_A ()
{
  return &gb2;
}

}

static int __attribute__ ((noinline, noclone))
get_a_number ()
{
  return 5;
}

extern "C" void abort (void);

int main (int argc, char *argv[])
{
  for (int i = 0; i < get_a_number (); i++)
    {
      struct A *p = get_an_A ();
      struct A *r = p->foo (4);
      if (r->fi != 1111)
	abort ();
      if (go != 22)
	abort ();
    }
  return 0;
}
