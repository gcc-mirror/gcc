// { dg-lto-do link }
// { dg-lto-options {{-flto -g}} }

extern "C" void abort (void);

class A
{
public:
  virtual int foo (int i);
};

int A::foo (int i)
{
  return i + 1;
}

int __attribute__ ((noinline,noclone)) get_input(void)
{
  return 1;
}

int main (int argc, char *argv[])
{

  class B : public A
  {
  public:
    int bar (int i)
    {
      return foo (i) + 2;
    }
  };
  class B b;

  if (b.bar (get_input ()) != 4)
    abort ();
  return 0;
}

