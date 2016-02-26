char a;
short b;
void fn1() {
  if (b)
    ;
  else {
    int c[1] = {0};
  l1:;
  }
  if (a)
    goto l1;
}
