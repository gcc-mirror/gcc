// PR c++/90810
// { dg-do run }

void
foo (float x, float y)
{
  typedef float __attribute__ ((__vector_size__ (4 * sizeof (float)), __may_alias__)) V;
  const V a = { x, x, x, x }, b = { y, y, y, y };
  const V c = a / b;
  if (c[0] != 6.0f || c[1] != 6.0f || c[2] != 6.0f || c[3] != 6.0f)
    __builtin_abort ();
}

void
bar (float y)
{
  typedef float __attribute__ ((__vector_size__ (4 * sizeof (float)), __may_alias__)) V;
  const V a = { 7.0f, 8.0f, 9.0f, 10.0f }, b = { 1.0f, 2.0f, 3.0f, y };
  const V c = a / b;
  if (c[0] != 7.0f || c[1] != 4.0f || c[2] != 3.0f || c[3] != 5.0f)
    __builtin_abort ();
}

int
main ()
{
  foo (12.0f, 2.0f);
  bar (2.0f);
}
