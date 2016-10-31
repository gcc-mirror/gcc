/* { dg-do compile } */
/* { dg-options "-Wshadow=compatible-local" } */

struct Bar {
};

struct Bar bar;       /* { dg-bogus "shadowed declaration" } */

int val;              /* { dg-bogus "shadowed declaration" } */

int func1(int x) {    /* { dg-bogus "shadowed declaration" } */
  int val;            /* { dg-bogus "shadows a global" } */
  val = x;
  return val;
}

int func2(int i) {
  int a = 3;          /* { dg-message "shadowed declaration" } */
  int j;              /* { dg-message "shadowed declaration" } */

  for (j = 0; j < i; ++j) {
    int a = j;        /* { dg-warning "shadows a previous local" } */
    int j = a + 1;    /* { dg-warning "shadows a previous local" } */
    func1(j);
  }

  return a;
}

void func4() {
  struct Bar bar;     /* { dg-bogus "shadowed declaration" } */
  if (val) {
    int bar;          /* { dg-bogus "shadows a previous local" } */
    func1(bar);
  }
}
