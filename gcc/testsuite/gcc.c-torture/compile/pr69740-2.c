inline int foo(int *p1, int p2) {
  int z = *p1;
  while (z > p2)
    p2 = 2;
  return z;
}
int main() {
  int i;
  for (;;) {
    int j, k;
    i = foo(&k, 7);
    if (k)
      j = i;
    else
      k = j;
    if (2 != j)
      __builtin_abort();
  }
}
