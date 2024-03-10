/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef __attribute__((__vector_size__(64))) int T;

void f(void) {
  extern char a[64], b[64];  /* { dg-message "previous" "note" } */
  void *p = a;
  T q = *(T *)&b[0];
}

void g() {
  extern char b;  /* { dg-error "conflicting types" } */
}
