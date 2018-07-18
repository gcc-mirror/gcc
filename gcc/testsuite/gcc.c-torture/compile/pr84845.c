/* PR target/84845 */

int a, b, c;
unsigned long d;

void
foo (void)
{
  b = -1;
  b <<= c >= 0;
  d = d << (63 & (short)-b) | d >> (63 & -(short)-b);
}
