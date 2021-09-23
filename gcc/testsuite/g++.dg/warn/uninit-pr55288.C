/* PR tree-optimization/55288 - Improve handling/suppression of
   maybe-uninitialized warnings
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

struct iterator
{
  operator int ();
  iterator operator++ (int);
};

void bar (int);

static void foo (int x)
{
  bar (x);          // { dg-bogus "uninitialized" }
}

int baz (iterator j, iterator end, int p)
{
  bool valid = false;
  int q;
  for (; j != end; j++) {
    if (p > j) {
      if (not valid)
	p = j;
      break;
    }
    else if (p == j) {
      valid = true;
      q = -1;
    }
    else {
      valid = true;
      q = j;
    }
  }

  if (valid)
    foo (q);

  return p;
}
