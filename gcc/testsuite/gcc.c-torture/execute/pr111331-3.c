int a;
int b;

int main() {
  int d = b+30;
  {
    int t;
    t = d < 29 ? 29 : ((d > 28) ? 28 : d);
    a = t;
  }
  volatile int t = a;
  if (a != 28)
    __builtin_abort();
  return 0;
}
