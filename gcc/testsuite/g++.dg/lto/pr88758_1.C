extern int a[];
int b;
int c;

void PreEvaluate(void) {
  b = 0;
  for (; b < 8; b++)
    a[b] = c * (b > 0 ? b - 1 : 0);
}
