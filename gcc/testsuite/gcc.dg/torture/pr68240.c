/* { dg-do compile } */

int a, b, f;

void
fn1 ()
{
  int c = 1, d, e = 1;
  a = 1; 
  for (; f;)
    b = (c && (d = (e && a)));
}
