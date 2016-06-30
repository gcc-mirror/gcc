/* PR middle-end/71693 */

unsigned short v;

void
foo (int x)
{
  v = ((((unsigned short) (0x0001 | (x & 0x0070) | 0x0100) & 0x00ffU) << 8)
       | (((unsigned short) (0x0001 | (x & 0x0070) | 0x0100) >> 8) & 0x00ffU));
}
