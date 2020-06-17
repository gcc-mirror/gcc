/* PR 90983/manual documents `-Wno-stack-usage` flag, but it is unrecognized
   { dg-do compile }
   { dg-options "-Wall -Wframe-larger-than=123 -Wno-frame-larger-than" } */

void f (void*);

void g (void)
{
  char a [1234];
  f (a);
}
