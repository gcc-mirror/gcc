/* { dg-do compile } */

int a[81];
int b, c;

void
fn1()
{
  int d = b;
  for (; c; --c)
    a[c + d] = a[c];
}
