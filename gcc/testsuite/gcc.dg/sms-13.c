/* PR target/83507 */
/* { dg-do compile } */
/* { dg-options "-O2 -fmodulo-sched -fno-tree-ter -fno-tree-coalesce-vars" } */

void
foo (unsigned short int x, unsigned char y)
{
  unsigned char *a = &y;
  unsigned short int b;
  int c;

  while (y < 3)
    {
      if (x != 0)
        ++y;
      ++y;
    }

  for (c = 0; c < 5; ++c)
    {
      int d = 1;
      d += b > x;
      y &= d;
    }

  do
    {
      c += y;
      x = c;
    }
  while (x != 0);
}
