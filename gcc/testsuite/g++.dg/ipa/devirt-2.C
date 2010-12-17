/* Verify that simple virtual calls using this pointer are converted
   to direct calls by ipa-cp.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-early-inlining -fno-inline -fdump-ipa-cp"  } */

extern "C" void abort (void);

class A
{
public:
  int data;
  virtual int foo (int i);
  int middleman (int i)
  {
    return foo (i);
  }
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

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

int main (int argc, char *argv[])
{
  class B b;
  int i;
  for (i = 0; i < get_input(); i++)
    if (b.middleman (get_input ()) != 3)
      abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "Discovered a virtual call to a known target.*B::foo"  "cp"  } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */
