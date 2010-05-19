/* Verify that virtual calls are inlined (ithout early inlining) even
   when their caller is itself indirectly inlined.  */
/* { dg-do run } */
/* { dg-options "-O3 -fdump-ipa-inline -fno-early-inlining -fno-ipa-cp"  } */

extern "C" void abort (void);

class A
{
public:
  int data;
  virtual int bar (int i);
  virtual int foo (int i);
};

class B : public A
{
public:
  virtual int bar (int i);
  virtual int foo (int i);
};

class C : public A
{
public:
  virtual int foo (int i);
};

int A::bar (int i)
{
  return i + 100 * i;
}

int A::foo (int i)
{
  return bar (i) + 1;
}

int B::bar (int i)
{
  return i + 100 * (i + 2);
}

int B::foo (int i)
{
  return bar (i) + 2;
}

int C::foo (int i)
{
  return i + 3;
}

int middleman (class A *obj, int i)
{
  return obj->foo (i);
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
    if (middleman (&b, get_input ()) != 303)
      abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "B::foo\[^\\n\]*inline copy in int main" "inline"  } } */
/* { dg-final { scan-ipa-dump "B::bar\[^\\n\]*inline copy in int main" "inline"  } } */
/* { dg-final { cleanup-ipa-dump "inline" } } */
