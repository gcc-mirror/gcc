/* { dg-do compile } */

int foo (char c, int i)
{
  int s = 0;
  while (i--)
    s += c;
  return s;
}

