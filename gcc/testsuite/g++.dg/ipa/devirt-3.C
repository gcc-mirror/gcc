/* Verify that simple virtual calls on an object refrence are
   converted to simple calls by ipa-cp.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-early-inlining -fno-inline -fdump-ipa-cp -fdump-tree-optimized"  } */

extern "C" void abort (void);

class A
{
public:
  int data;
  virtual float distraction (float f);
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

float A::distraction (float f)
{
  f += 6.2;
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

static int middleman (class A &obj, int i)
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
  if (middleman (b, get_input ()) != 3)
    abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "Discovered a virtual call to a known target.*B::foo"  "cp"  } } */
/* { dg-final { scan-tree-dump-times "OBJ_TYPE_REF" 0 "optimized"} } */
/* { dg-final { cleanup-ipa-dump "cp" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
