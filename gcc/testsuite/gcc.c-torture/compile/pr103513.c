int a;
void b(int c) {
  int d = 3;
  d ^= c < 2;
  if (d < 3 && a)
    while (1)
      b(!a);
}
