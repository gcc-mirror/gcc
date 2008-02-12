int f(int a, int b, short c, int d, short e)
{
  int i;
  for (i = 1; i <= 2 ; i++) {
    c -= 4;
    a = c;
    d = d + (b?b:e);
  }
  return a + d;
}
