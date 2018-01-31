// PR c++/83993
// { dg-do compile }

extern const int a[];
const int *const b = &a[0];

int
foo ()
{
  return b[0];
}
