/* { dg-options "-g" } */

void
foo (int x)
{
  struct S { int s; } d = { 1 };
  unsigned int e = 1;
  if (x)
    e = x && d.s;
  else
    for (e = 0; e <= 3; e--)
      ;
  e++;
}
