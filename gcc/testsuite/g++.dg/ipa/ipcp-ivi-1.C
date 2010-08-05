/* Verify that simple virtual calls are inlined even without early
   inlining.  */
/* { dg-do run } */
/* { dg-options "-O3 -fdump-ipa-inline -fno-early-inlining"  } */

extern "C" void abort (void);

class A
{
public:
  int data;
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

int __attribute__ ((noinline)) middleman (class A *obj, int i)
{
  return obj->foo (i);
}

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

class B b;

int main (int argc, char *argv[])
{
  int i;

  for (i = 0; i < get_input (); i++)
    if (middleman (&b, get_input ()) != 3)
      abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "B::foo\[^\\n\]*inline copy in int.*middleman"  "inline"  } } */
/* { dg-final { cleanup-ipa-dump "inline" } } */
