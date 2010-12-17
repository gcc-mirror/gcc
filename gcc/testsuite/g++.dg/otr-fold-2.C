/* Verify that virtual calls are folded even when a typecast to an
   ancestor is involved along the way.  */
/* { dg-do run } */
/* { dg-options "-O -fdump-tree-optimized-slim"  } */

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

class A_2 : public A
{
public:
  int data_2;
  virtual int baz (int i);
};


class B : public Distraction, public A_2
{
public:
  virtual int foo (int i);
};

float Distraction::bar (float z)
{
  f += z;
  return f/2;
}

int A::foo (int i)
{
  return i + 1;
}

int A_2::baz (int i)
{
  return i * 15;
}

int B::foo (int i)
{
  return i + 2;
}

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

static inline int middleman_1 (class A *obj, int i)
{
  return obj->foo (i);
}

static inline int middleman_2 (class A *obj, int i)
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

/* { dg-final { scan-tree-dump "= B::.*foo"  "optimized"  } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
