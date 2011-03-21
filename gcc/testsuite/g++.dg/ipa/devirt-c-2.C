/* Verify that ipa-cp correctly detects the dynamic type of an object
   under construction when doing devirtualization.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-early-inlining -fno-inline"  } */

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
  A();
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

static int middleman (class A *obj, int i)
{
  return obj->foo (i);
}

A::A()
{
  if (middleman (this, get_input ()) != 2)
    abort ();
}

static void bah ()
{
  class B b;
}

int main (int argc, char *argv[])
{
  int i;

  for (i = 0; i < 10; i++)
    bah ();
  return 0;
}
