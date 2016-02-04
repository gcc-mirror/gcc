/* { dg-do compile } */
/* { dg-options "-O3" } */

int a, d;
char b, c;

void
fn1 ()
{
  int i = 0;
  for (; i < 1; i++)
    d = 1;
  for (; b; b++)
    a = 1 && (d & b);
}
