/* { dg-do compile } */

typedef __attribute__((__vector_size__ (8))) double V;
int bar (int a, V *p)
{
  V v;
  v = *p;
  a += *(_Complex short *) &v;
  return a;
}
V x;
int
foo ()
{
  return bar (0, &x);
}
