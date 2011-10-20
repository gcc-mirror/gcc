// PR c++/50793
// { dg-do run }

struct NonTrivial
{
  NonTrivial() { }
};

struct S
{
  NonTrivial nt;
  int i;
};

int f(S s)
{
  s.i = 0xdeadbeef;
  return s.i;
}

int g(S s = S())
{
  return s.i;
}

int main()
{
  f(S());  // make stack dirty

  if ( g() )
    __builtin_abort();
}
