int *a;
int foo() {
  if (a && a - (int *)0 > 0)
    return 0;
}
