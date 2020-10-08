/* { dg-do compile } */
/* { dg-options "-O2" } */

void c(int);

int a;
void b()
{
  if (a >= 2147483647)
    c(a + 1);
}
