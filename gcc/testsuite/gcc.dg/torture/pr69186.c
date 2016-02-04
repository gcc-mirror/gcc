/* { dg-do compile } */

static int a[10][2];
int b;

void
fn1 ()
{
  b = 0;
  for (; b < 6; b++)
    a[b][2] ^= 1;
}
