// PR middle-end/119613
// { dg-do compile { target { musttail && c++11 } } }
// { dg-options "-O0" }

struct S { S () {} };
char *foo (S);
void bar (int);

[[gnu::always_inline]] inline char *
baz (S x)
{
  unsigned i;
  &i;
  bar (i);
  [[gnu::musttail]] return foo (x);
}

char *
qux (S)
{
  [[gnu::musttail]] return baz (S {});
}
