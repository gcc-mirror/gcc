/* Verify that ipa-cp correctly detects the dynamic type of an object
   under construction when doing devirtualization.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-inline"  } */

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
  B();
  virtual int foo (int i);
};

class C : public B
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

int C::foo (int i)
{
  return i + 3;
}

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

static int __attribute__ ((noinline))
middleman (class A *obj, int i)
{
  return obj->foo (i);
}

static void __attribute__ ((noinline))
sth2 (A *a)
{
  if (a->foo (get_input ()) != 3)
    abort ();
}

inline void __attribute__ ((always_inline)) sth1 (B *b)
{
  sth2 (b);
}

inline __attribute__ ((always_inline)) A::A()
{
  if (middleman (this, get_input ()) != 2)
    abort ();
}

B::B() : Distraction(), A()
{
  sth1 (this);
}

static void bah ()
{
  class C c;
}

int main (int argc, char *argv[])
{
  int i;

  for (i = 0; i < 10; i++)
    bah ();
  return 0;
}
