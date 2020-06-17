// PR c++/50793
// { dg-do run }

typedef int int32_t __attribute__((mode (__SI__)));

struct NonTrivial
{
  NonTrivial() { }
};

struct S
{
  NonTrivial nt;
  int32_t i;
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
