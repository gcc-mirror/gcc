/* PR 90983/manual documents `-Wno-stack-usage` flag, but it is unrecognized
   { dg-do compile }
   { dg-options "-Wall -Wstack-usage=123 -Wno-stack-usage" } */

void f (void*);

void g (int n)
{
  if (n < 1234)
    n = 1234;

  char a [n];
  f (a);
}
