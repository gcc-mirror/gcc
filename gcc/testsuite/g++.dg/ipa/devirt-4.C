/* Verify that ipa-co can convert virtual calls to direct ones even
   when a typecast to an ancestor is involved along the way.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-early-inlining -fno-inline -fdump-ipa-cp -fdump-tree-optimized"  } */

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

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

static int middleman_1 (class A *obj, int i)
{
  return obj->foo (i);
}

static int middleman_2 (class B *obj, int i)
{
  return middleman_1 (obj, i);
}

int main (int argc, char *argv[])
{
  class B b;
  if (middleman_2 (&b, get_input ()) != 3)
    abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "Discovered a virtual call to a known target.*B::foo"  "cp"  } } */
/* { dg-final { scan-tree-dump-times "OBJ_TYPE_REF" 0 "optimized"} } */
