// { dg-lto-do run }
// { dg-require-linker-plugin "" }
// { dg-lto-options {{-O2 -fuse-linker-plugin -fno-early-inlining}}

extern "C" void abort (void);
extern "C" void linker_error ();

class A
{
public:
  int data;
  virtual int foo (int i)
    {
      return i + 1;
    }
};

class B : public A
{
public:
  virtual int foo (int i)
    {
      return i + 2;
    }
};

class C : public A
{
public:
  virtual int foo (int i)
    {
      linker_error ();
      return i + 3;
    }
};


static int middleman (class A *obj, int i)
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
  if (middleman (&b, get_input ()) != 3)
    abort ();
  return 0;
}
