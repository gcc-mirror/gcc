void (*p)();

void f() {
  (void *)p; // { dg-error "" }
}
