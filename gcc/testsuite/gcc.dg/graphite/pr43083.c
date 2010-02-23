/* { dg-options "-O3 -fgraphite-identity" } */

extern void baz(void);

static inline int bar(void)
{
  int i;
  for (i = 0; i < 10; i++) baz();
}

int foo(void)
{
  if (bar() != 0) return 0;
}
