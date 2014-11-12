int a, b;
short c;

void fn1 ();

void
fn2 (unsigned short p1)
{
  int d;

  c = p1 >> 8 | p1 << 8;
  d = b;
  if (d)
    fn1 ();
  a = d >> 8 & 0x00FF
    | d << 8 & 0xFF00;
}
