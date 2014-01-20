/* { dg-do compile } */

int a[7];
int b;

void
fn1 ()
{
  for (; b; b++)
    a[b] = ((a[b] <= 0) == (a[0] != 0));
}

int
main ()
{
  return 0;
}
