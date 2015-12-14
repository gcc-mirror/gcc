// { dg-do run }
// { dg-options "-O2 -fipa-cp-clone" }

struct Distraction
{
  char fc[8];
  virtual Distraction * return_self ()
  { return this; }
};

static int go;

struct A;

struct A
{
  int fi;

  A () : fi(0) {}
  A (int pi) : fi (pi) {}
  virtual void foo (int p) = 0;
};

struct B;

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

struct B gb2 (2);

extern "C" void abort (void);

int
main (void)
{
  for (int i = 0; i < 2; i++)
    {
      struct A *p = &gb2;
      p->foo (0);
      if (go != 2)
	abort ();
    }
  return 0;
}
