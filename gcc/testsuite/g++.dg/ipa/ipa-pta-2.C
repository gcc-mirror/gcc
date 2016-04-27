// { dg-do run }
// { dg-options "-O2 -fipa-pta" }

extern "C" void abort (void);

struct Y { ~Y(); int i; };

Y::~Y () {}

static Y __attribute__((noinline)) foo ()
{
  Y res;
  res.i = 3;
  return res;
}

static Y __attribute__((noinline)) bar ()
{
  Y res;
  res.i = 42;
  return res;
}

static Y (*fn) ();

int a;
int main()
{
  if (a)
    fn = foo;
  else
    fn = bar;
  Y res = fn ();
  if (res.i != 42)
    abort ();
  return 0;
}
