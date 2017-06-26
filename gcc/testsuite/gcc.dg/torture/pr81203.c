/* { dg-do compile } */

int a;
int b()
{
  int c, d;
  if (a)
    d = b();
  return 1 + c + d;
}
