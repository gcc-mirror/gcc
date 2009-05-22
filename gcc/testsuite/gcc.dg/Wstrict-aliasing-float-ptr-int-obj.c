/* { dg-do compile } */
/* { dg-options "-O -Wstrict-aliasing -fstrict-aliasing" } */

extern int flag;

int foo() {

  int x;
  int y = 9;
  float* q;
  float* r;

  if (flag) {
    q = (float*) &x;  /* { dg-message "initialized" "" { xfail *-*-* } } */
  } else {
    q = (float*) &y;  /* { dg-message "initialized" "" { xfail *-*-* } } */
  }

  *q = 1.0;  /* { dg-warning "does break strict-aliasing" "" { xfail *-*-* } } */

  return x;

}
