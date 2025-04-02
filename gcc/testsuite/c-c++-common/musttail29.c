/* { dg-do compile { target { musttail && { c || c++11 } } } } */
/* { dg-options "-O2 -Wmusttail-local-addr" } */

int foo (int, void *);
int bar (int, int *);
struct S { int a, b, c; };
struct T { int d; struct S e; };

int
baz (int x, void *y)
{
  [[gnu::musttail]] return bar (2, &x);		/* { dg-warning "address of parameter 'x' passed to 'musttail' call argument" } */
}

int
qux (int x, void *y)
{
  __label__ lab;
  lab:;
  if (*(int *) y == 1)
    [[gnu::musttail]] return foo (1, &&lab);	/* { dg-warning "address of label passed to 'musttail' call argument" } */
  if (x == 1)
    [[gnu::musttail]] return foo (3, 0);
  else if (x == 2)
    {
      {
        int a = 42;
        bar (4, &a);
      }
      [[gnu::musttail]] return bar (5, 0);
    }
  else if (x == 3)
    {
      int a = 42;
      bar (4, &a);
      [[gnu::musttail]] return bar (6, 0);
    }
  else if (x == 4)
    {
      int a = 42;
      [[gnu::musttail]] return bar (7, &a);	/* { dg-warning "address of automatic variable 'a' passed to 'musttail' call argument" } */
    }
  else if (x == 5)
    {
      struct T b;
      [[gnu::musttail]] return bar (8, &b.e.b);	/* { dg-warning "address of automatic variable 'b' passed to 'musttail' call argument" } */
    }
  else if (x == 6)
    {
      struct T b;
      bar (9, &b.e.a);
      [[gnu::musttail]] return bar (10, 0);
    }
  else if (x == 7)
    {
      {
        struct T b;
        bar (9, &b.e.a);
      }
      [[gnu::musttail]] return bar (11, 0);
    }
  else if (x == 8)
    {
      {
        int a = 42;
        bar (4, &a);
      }
      [[gnu::musttail]] return foo (12, 0);
    }
  else if (x == 9)
    {
      int a = 42;
      bar (4, &a);
      [[gnu::musttail]] return foo (13, 0);
    }
  else if (x == 10)
    {
      int a = 42;
      [[gnu::musttail]] return foo (14, &a);	/* { dg-warning "address of automatic variable 'a' passed to 'musttail' call argument" } */
    }
  else if (x == 11)
    {
      struct T b;
      [[gnu::musttail]] return foo (15, &b.e.b); /* { dg-warning "address of automatic variable 'b' passed to 'musttail' call argument" } */
    }
  else if (x == 12)
    {
      struct T b;
      bar (9, &b.e.a);
      [[gnu::musttail]] return foo (16, 0);
    }
  else if (x == 13)
    {
      {
        struct T b;
        bar (9, &b.e.a);
      }
      [[gnu::musttail]] return foo (17, 0);
    }
  return 0;
}

int
corge (int x, void *y)
{
  if (*(int *) y == 1)
    bar (18, &x);
  [[gnu::musttail]] return bar (2, 0);
}
