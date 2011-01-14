/* Verify that ipa-cp correctly detects the dynamic type of an object
   under construction when doing devirtualization.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-inline"  } */

extern "C" void abort (void);

class A
{
public:
  int data;
  A();
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

static inline int __attribute__ ((always_inline))
middleman (class A *obj, int i)
{
  return obj->foo (i);
}

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

__attribute__ ((noinline)) A::A ()
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
