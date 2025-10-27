/* { dg-do compile } */

int a;
long b;
void c()
{
  for (; b; b--)
    a = (char)a;
}
