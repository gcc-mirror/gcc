// { dg-do run }
// { dg-options "-O3 -fdump-ipa-cp" }

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
  virtual void foo (int p) = 0;
};

struct B : public Distraction, A
{
  B () : Distraction(), A() { }
  B (int pi) : Distraction (), A (pi) {}
  virtual void foo (int p)
  {
    int o = fi;
    for (int i = 0; i < p; i++)
      o += i + i * i;
    go = o;
  }
};


struct B gb (2);
static A * __attribute__ ((noinline, noclone))
get_an_A ()
{
  return &gb;
}

}

static int __attribute__ ((noinline, noclone))
get_a_number ()
{
  return 5;
}

extern "C" void abort (void);

static void __attribute__ ((noinline, noclone))
bar ()
{
  for (int i = 0; i < get_a_number (); i++)
    {
      struct A *p = get_an_A ();
      p->foo (4);
      if (go != 22)
	abort ();
    }
}

int main (int argc, char *argv[])
{
  for (int i = 0; i < get_a_number (); i++)
    {
      struct A *p = get_an_A ();
      p->foo (4);
      if (go != 22)
	abort ();
    }

  bar ();
  return 0;
}

/* { dg-final { scan-ipa-dump-times "Thunk fixed offset" 2 "cp"} } */
