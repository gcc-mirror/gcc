/* Verify that virtual calls are folded even early inlining puts them into one
   function with the definition.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-fre1-details"  } */

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
  __attribute__ ((noinline)) B();
  virtual int foo (int i);
};

int __attribute__ ((noinline)) A::foo (int i)
{
  return i + 1;
}

int __attribute__ ((noinline)) B::foo (int i)
{
  return i + 2;
}

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

__attribute__ ((noinline)) B::B()
{
}

static inline int middleman_1 (class A *obj, int i)
{
  return obj->foo (i);
}

static inline int middleman_2 (class B *obj, int i)
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

/* { dg-final { scan-tree-dump "converting indirect call to function virtual int B::foo" "fre1"  } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
