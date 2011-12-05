// PR debug/46338
// { dg-do compile }
// { dg-require-profiling "-fprofile-generate" }
// { dg-options "-O -fprofile-generate -fcompare-debug" }

void bar ();

struct S
{
  int f ()
  {
  }
};

S *s;

void
foo (int x)
{
  if (x)
    return;
  bar ();
  for (int j = 0; j < s->f (); j++)
    ;
}
