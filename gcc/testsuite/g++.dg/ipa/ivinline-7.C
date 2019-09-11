/* Verify that simple virtual calls are inlined even without early
   inlining, even when a typecast to an ancestor is involved along the
   way and that ancestor is not the first one with virtual functions.  */
/* { dg-do run { target { nonpic || pie_enabled } } } */
/* { dg-options "-O3 -fdump-ipa-inline -fno-early-inlining -fno-ipa-cp"  } */

extern "C" void abort (void);

class Distraction
{
public:
  float f;
  double d;
  Distraction ()
  {
    f = 8.3;
    d = 10.2;
  }
  virtual float bar (float z);
};

class A
{
public:
  int data;
  virtual int foo (int i);
};


class B : public Distraction, public A
{
public:
  virtual int foo (int i);
};

float Distraction::bar (float z)
{
  f += z;
  return f/2;
}

int A::foo (int i)
{
  return i + 1;
}

int B::foo (int i)
{
  return i + 2;
}

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

static int middleman_1 (class A *obj, int i)
{
  return obj->foo (i);
}

static int middleman_2 (class B *obj, int i)
{
  return middleman_1 (obj, i);
}

int main (int argc, char *argv[])
{
  class B b;
  int i;

  for (i = 0; i < get_input (); i++)
    if (middleman_2 (&b, get_input ()) != 3)
      abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "Discovered a virtual call to a known target.*B::.*foo"  "inline"  } } */
/* { dg-final { scan-ipa-dump "B::foo\[^\\n\]*inline copy in int main"  "inline"  } } */
