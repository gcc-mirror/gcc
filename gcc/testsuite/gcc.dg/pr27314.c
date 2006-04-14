/* { dg-do compile } */
/* { dg-options "-O1 -ffast-math" } */

extern double floor (double);

inline int bar (double x)
{
  return (int) floor (x);
}

int foo (int i)
{
  return bar (i);
}
