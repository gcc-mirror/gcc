/* Verify that virtual call inlining does not pick a wrong method when
   there is a user defined ancestor in an object.  */
/* { dg-do run { target { nonpic || pie_enabled } } } */
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
  class A confusion;
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
  int i, j = get_input ();

  for (i = 0; i < j; i++)
    if ((middleman (&b, j) + 100 * middleman (&b.confusion, j)) != 203)
      abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "A::foo\[^\\n\]*inline copy in int main"  "inline"  } } */
/* { dg-final { scan-ipa-dump "B::foo\[^\\n\]*inline copy in int main"  "inline"  } } */
