/* { dg-do compile } */

struct S { int s; };

void
foo (void)
{
  for (;;)
    ;
}

struct S
bar (void)
{
  struct S s = { 99 };
  return s;
}

void
baz (int i)
{
  struct S s[1];
  s[0] = bar ();
  bar ();
  foo ();
}

