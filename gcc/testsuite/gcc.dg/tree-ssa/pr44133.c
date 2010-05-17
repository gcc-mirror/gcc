/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

struct S { int i, j; };

int foo (int l)
{
  struct S s;
  s.j = l - 22;   /* { dg-warning ".s\.i. is used uninitialized" } */
  return s.i + s.j;
}
