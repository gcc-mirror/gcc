// { dg-do run }
// { dg-options "-O1" }

int a, b;
int main() {
  int c = 0;
  long d;
  for (; b < 1; b++) {
    (c && d) & 3 || a;
    d = c;
    c = -1;
    if (d)
      __builtin_abort();
  }
  return 0;
}
