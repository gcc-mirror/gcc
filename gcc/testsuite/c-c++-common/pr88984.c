/* PR c++/88984 */
/* { dg-do run } */

void
foo (int x, int y)
{
  while (x > 0)
    switch (({ if (y) break; y; }))
      {
      case 2: x = 0;
      }
}

int
main ()
{
  foo (1, 1);
  return 0;
}
