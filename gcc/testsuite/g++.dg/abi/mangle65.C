// PR c++/64898
// { dg-final { scan-assembler-not "_Z6foovar" } }

template <class> void f()
{
  extern int foovar;
  foovar = 42;
}

int main()
{
  f<int>();
}
