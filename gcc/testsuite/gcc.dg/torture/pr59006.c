/* { dg-do compile } */

int a[8], b;
void fn1(void)
{
  int c;
  for (; b; b++)
    {
      int d = a[b];
      c = a[0] ? d : 0;
      a[b] = c;
    }
}
