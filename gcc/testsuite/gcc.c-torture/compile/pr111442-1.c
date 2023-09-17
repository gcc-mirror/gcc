
int *a, b;
int main() {
  int d = 1, e;
  if (d)
    e = a ? 0 % 0 : 0;
  if (d)
    a = &d;
  d = -1;
  b = d & e;
  b = 2 * e ^ 1;
  return 0;
}
