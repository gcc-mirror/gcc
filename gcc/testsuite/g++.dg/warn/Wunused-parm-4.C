// PR c++/47783
// { dg-do compile }
// { dg-options "-Wunused -W" }

struct R
{
  int &i;
};

void
foo (R r, int &s)
{
  r.i = 7;
  s = 8;
}

int
bar ()
{
  int x = 1, y = 1;
  R r = { x };
  foo (r, y);
  return x + y;
}
