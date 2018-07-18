// PR c++/79232
// { dg-do compile }

extern char a[];
int b;
char c, e;

void
foo (long d)
{
  (0, b ? &c : a)[d] = e;
}
