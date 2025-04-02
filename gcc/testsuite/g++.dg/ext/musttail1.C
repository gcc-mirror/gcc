// PR ipa/119376
// { dg-do compile { target { musttail && c++11 } } }
// { dg-options "-Wmaybe-musttail-local-addr" }

int foo (int &);
int bar (int &&);
int corge (int *);

int
baz (int &x)
{
  if (x == 1)
    [[gnu::musttail]] return foo (x);
  if (x == 2)
    {
      int a = 42;
      [[gnu::musttail]] return foo (a);		// { dg-warning "address of automatic variable 'a' passed to 'musttail' call argument" }
    }
  if (x == 3)
    {
      int a = 42;
      foo (a);
      [[gnu::musttail]] return foo (x);		// { dg-warning "address of automatic variable 'a' can escape to 'musttail' call" }
    }
  return 0;
}

int
qux (int &&x)
{
  [[gnu::musttail]] return bar (x + 1);		// { dg-warning "address of local variable passed to 'musttail' call argument" }
}

int
freddy (int x)
{
  [[gnu::musttail]] return foo (x);		// { dg-warning "address of parameter 'x' passed to 'musttail' call argument" }
}
