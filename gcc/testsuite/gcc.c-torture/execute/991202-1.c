void exit (int);

int x, y;

int
main()
{
  x = 2;
  y = x;
  do
    {
      x = y;
      y = 2 * y;
    }
  while ( ! ((y - x) >= 20));
  exit (0);
}

