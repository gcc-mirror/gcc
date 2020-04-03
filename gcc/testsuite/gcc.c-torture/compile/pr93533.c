/* PR target/93533 */

unsigned
foo (unsigned short a)
{
  a = a - (a >> 1 & 21845);
  a = (a & 13107) + (a >> 2 & 13107);
  return (unsigned short) ((a + (a >> 4) & 3855) * 257) >> 8;
}
