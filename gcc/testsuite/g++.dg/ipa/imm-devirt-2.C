/* Verify that virtual calls are folded even early inlining puts them into one
   function with the definition.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-fre1-details"  } */

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

class B : public A
{
public:
  int data_2;
  virtual int foo (int i);
  virtual int baz (int i);
};


class C : public Distraction, public B
{
public:
  __attribute__ ((noinline)) C();
  virtual int foo (int i);
};

float __attribute__ ((noinline)) Distraction::bar (float z)
{
  f += z;
  return f/2;
}

int __attribute__ ((noinline)) A::foo (int i)
{
  return i + 1;
}

int __attribute__ ((noinline)) B::foo (int i)
{
  return i + 2;
}

int __attribute__ ((noinline)) B::baz (int i)
{
  return i * 15;
}

int __attribute__ ((noinline)) C::foo (int i)
{
  return i + 3;
}

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

static inline int middleman (class A *obj, int i)
{
  return obj->foo (i);
}

__attribute__ ((noinline)) C::C()
{
}

int main (int argc, char *argv[])
{
  class C c;

  if (middleman (&c, get_input ()) != 4)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "converting indirect call to function" "fre1"  } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
