/* PR target/52375 */

struct C { int c, d; };

unsigned
foo (struct C *p)
{
  unsigned int b = 0, i;
  for (i = 0; i < 64; i++)
    b |= 0x80000000U >> p[i].c;
  return b;
}
