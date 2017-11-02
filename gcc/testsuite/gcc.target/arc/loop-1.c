/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Check how we handle empty body loops.  */

int a;
void fn1(void) {
  int i;
  for (; i < 8; i++) {
    double A[a];
  }
}
