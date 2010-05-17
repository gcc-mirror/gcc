/* Verify that simple virtual calls on an object refrence are inlined
   even without early inlining.  */
/* { dg-do run } */
/* { dg-options "-O3 -fdump-ipa-inline -fno-early-inlining -fno-ipa-cp"  } */

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

int middleman (class A &obj, int i)
{
  return obj.foo (i);
}

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

int main (int argc, char *argv[])
{
  class B b;
  int i;

  for (i = 0; i < get_input (); i++)
    if (middleman (b, get_input ()) != 3)
      abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "B::foo\[^\\n\]*inline copy in int main"  "inline"  } } */
/* { dg-final { cleanup-ipa-dump "inline" } } */
