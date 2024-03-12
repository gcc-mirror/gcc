
#define comparison (f < 0)
int main() {
  int f = 0;
  int d = comparison | !comparison;
  if (d != 1)
    __builtin_abort();
  return 0;
}

