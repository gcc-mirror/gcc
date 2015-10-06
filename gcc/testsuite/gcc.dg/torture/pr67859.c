/* { dg-do compile } */

int a, b, c;

void
fn1 ()
{
  b = c ? 0 : 1 << a;
  b |= 0x9D7A5FD9;
  for (;;)
    {
      int d = 1;
      b &= (unsigned) d;
    }
}
