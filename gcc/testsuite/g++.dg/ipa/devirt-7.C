/* Verify that IPA-CP can do devirtualization even if the virtual call
   comes from a method that has been early-inlined into a descendant.  */
/* { dg-do run } */
/* { dg-options "-O3 -fdump-ipa-cp"  } */

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
  int middleman_1 (int i);
};


class B : public Distraction, public A
{
public:
  virtual int foo (int i);
  int middleman_2 (int i);
  __attribute__ ((noinline)) B();
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

int inline __attribute__ ((always_inline))
A::middleman_1 (int i)
{
  return this->foo (i);
}

int __attribute__ ((noinline))
B::middleman_2 (int i)
{
  return this->middleman_1 (i);
}

B::B ()
{
}

int main (int argc, char *argv[])
{
  class B b;
  int i;

  for (i = 0; i < get_input(); i++)
    if (b.middleman_2 (get_input ()) != 3)
      abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "Discovered a virtual call to a known target.*B::foo"  "cp"  } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */
