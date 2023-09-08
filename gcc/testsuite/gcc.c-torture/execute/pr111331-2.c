
int a;
int b;

int main() {
  int d = b+30;
  {
        int t;
        if (d < 29)
          t =  29;
        else
          t = (d > 28) ? 28 : d;
    a = t;
  }
  volatile int t = a;
  if (a != 28)
    __builtin_abort();
  return 0;
}
