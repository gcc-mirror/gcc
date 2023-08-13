/* { dg-do compile } */
/* { dg-options "-O2" } */

struct A { char e, f; };

struct B
{
  int g;
  struct A h[4];
};

extern void bar (int, int);

struct B foo (void)
{
  bar (2, 1);
}

void baz ()
{
  foo ();
}
