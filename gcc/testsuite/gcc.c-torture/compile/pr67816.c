int a, c, d, e;
int b[10];
void fn1() {
  int i, f = 0;
  for (;;) {
    i = 0;
    for (; i < a; i++)
      if (c) {
        if (b[i])
          f = 1;
      } else if (b[i])
        f = 0;
    if (f)
      d++;
    while (e)
      ;
  }
}

