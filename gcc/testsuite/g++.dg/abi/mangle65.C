// PR c++/64898
// { dg-final { scan-assembler-not "_Z6foovar" } }
// { dg-additional-options -fabi-compat-version=0 }

template <class> void f()
{
  extern int foovar;
  foovar = 42;
}

int main()
{
  f<int>();
}
