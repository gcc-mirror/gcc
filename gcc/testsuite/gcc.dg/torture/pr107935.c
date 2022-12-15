/* { dg-do compile } */

int *a, **b;
int main() {
  int d = 0, *e = &d;
 L:
  *e = d;
  if (a) {
    int *g = e = *b;
    if (!e)
      __builtin_abort();
    if (**b)
      return 0;
    *g = 1;
    goto L;
  }
  return 0;
}
