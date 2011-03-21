/* { dg-do compile } */

void f(char *s)
{
  signed short i;

  for (i = 0; i < 19; i = i + 1)
    s[i] = i;
}
