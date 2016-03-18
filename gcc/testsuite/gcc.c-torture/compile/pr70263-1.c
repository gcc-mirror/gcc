int a[91];
int b, c;
void fn1() {
  int n, m;
  do {
    a[c--];
    a[--c] = m;
    a[--m] = b;
  } while (n);
}

