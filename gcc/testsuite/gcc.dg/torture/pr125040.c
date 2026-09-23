/* { dg-do compile } */

int a() {
  int b;
  int *c = __builtin_calloc(sizeof(int), b);
  int count;
  for (int d; d;)
    for (int e;; b++) {
      c[e] = c[b - count - 1];
      c[b - count - 1] = count;
      count = count + 1;
    }
  __builtin_printf("", c);
}
