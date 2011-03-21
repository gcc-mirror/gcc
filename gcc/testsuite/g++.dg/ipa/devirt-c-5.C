/* Verify that ipa-cp correctly detects the dynamic type of an object
   under construction when doing devirtualization.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-early-inlining -fno-inline"  } */

extern "C" void abort (void);

class B;

class A
{
public:
  int data;
  A();
  A(B *b);
  virtual int foo (int i);
};

class B : public A
{
public:
  virtual int foo (int i);
};

class C : public A
{
public:
  virtual int foo (int i);
};

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

static int middleman (class A *obj, int i)
{
  return obj->foo (i);
}

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

A::A ()
{
}

A::A (B *b)
{
  if (middleman (b, get_input ()) != 3)
    abort ();
}

static void bah ()
{
  B b;
  A a(&b);
}

int main (int argc, char *argv[])
{
  int i;

  for (i = 0; i < 10; i++)
    bah ();
  return 0;
}
