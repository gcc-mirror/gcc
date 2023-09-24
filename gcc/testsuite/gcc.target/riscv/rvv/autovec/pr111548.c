/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -Ofast -Wno-implicit-int -Wno-implicit-function-declaration -Wno-int-conversion" } */

a, c, d; /* { dg-warning "data definition has no type or storage class" } */
*b; /* { dg-warning "data definition has no type or storage class" } */
h() {
  int e;
  i();
  for (;;) {
    unsigned f;
    char *g;
    f = a;
    for (; f; f--) {
      if (*g == '"')
        e = !e;
      *b = g++;
    }
    if (c)
      break;
    f = d;
    for (; d;)
      if (e)
        b++;
  }
}
