// PR optimization/6189
// Bug: we forgot about foo's nrv after writing it out.
// { dg-options -O3 }
// { dg-do run }

struct A
{
  int i;
};


A foo ()
{
  A a;
  a.i = 42;
  return a;
}


int main()
{
  A b = foo();
  return b.i != 42;
}
