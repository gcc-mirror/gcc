/* { dg-do compile } */

enum { a = 1 } b;
int c()
{
  int d = a;
  for (; d;)
    d &= d - 1;
  return b;
}
