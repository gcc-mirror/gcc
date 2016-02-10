int a[32];
int fn1(int d) {
  int c = 1;
  for (int b = 0; b < 32; b++)
    if (a[b])
      c = 0;
  return c;
}
