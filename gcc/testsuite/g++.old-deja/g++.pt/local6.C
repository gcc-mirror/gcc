extern "C" void abort();

template <class T>
int f(T)
{
  struct S1 {
    virtual int foo() { return 1; }
  };

  struct S2 : public S1 {
    int foo() { return 2; }
  };

  S1* s2 = new S2;

  return s2->foo();
}


int main()
{
  if (f(3) != 2)
    abort();
}
