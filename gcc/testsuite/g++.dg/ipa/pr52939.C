/* Verify that we do not ICE on invalid devirtualizations (which might
   be OK at run-time because never executed).  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-early-inlining -fno-inline"  } */

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
  virtual int bar (int i);
};

int A::foo (int i)
{
  return i + 1;
}

int B::foo (int i)
{
  return i + 2;
}

int B::bar (int i)
{
  return i + 3;
}

static int middleman (class A *obj, int i)
{
  class B *b = (class B *) obj;

  if (i != 1)
    return b->bar (i);
  else
    return i;
}

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

int main (int argc, char *argv[])
{
  class A o;
  if (middleman (&o, get_input ()) != 1)
    abort ();
  return 0;
}
