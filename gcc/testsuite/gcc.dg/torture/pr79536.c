/* { dg-do compile } */

typedef int A;
int
fn1 (A x, A y)
{
  if ((x + (x - y) * 1i) != -(-x + (y - x) * 1i))
    return 1;
  return 0;
}
