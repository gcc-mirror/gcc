int f2 (int, int);
int f3 (int);
int
f1 (int a, int b)
{
  int i, j, k;

  switch (b)
    {
    case (-9):
      j = 4;
      break;
    case (-10):
      j = 10;
      break;
    case (-8):
      j = 15;
      break;
    }

  i = f2 (f3 (b == (-9) ? k : a), j);

  return 0;
}
