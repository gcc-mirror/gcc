/* { dg-do compile } */
/* { dg-options "-O3 -fno-early-inlining -fno-ipa-cp"  } */

extern "C" void abort (void);

class A
{
public:
  virtual int foo (int i);
};

class B
{
public:
  class A confusion;
};

int A::foo (int i)
{
  return i + 1;
}

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

static int middleman_a (class A *obj, int i)
{
  return obj->foo (i);
}

static int middleman_b (class B *obj, int i)
{
  return middleman_a (&obj->confusion, i);
}


int main (int argc, char *argv[])
{
  class B b;
  int i, j = get_input ();

  for (i = 0; i < j; i++)
    if (middleman_b (&b, j) != 2)
      abort ();
  return 0;
}
