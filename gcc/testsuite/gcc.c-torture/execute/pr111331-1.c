int a;
int b;
int c(int d, int e, int f) {
  if (d < e)
    return e;
  if (d > f)
    return f;
  return d;
}
int main() {
  int g = -1;
  a = c(b + 30, 29, g + 29);
  volatile int t = a;
  if (t != 28)
    __builtin_abort();
  return 0;
}
