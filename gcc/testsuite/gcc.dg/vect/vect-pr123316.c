/* { dg-do compile } */

int a;
extern bool b[];
void e (int c)
{
  for (; c; c++)
    for (int d = 2; d < 21; d++)
      b[c] |= a;
}
