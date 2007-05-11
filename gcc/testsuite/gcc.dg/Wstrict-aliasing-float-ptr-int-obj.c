/* { dg-do compile } */
/* { dg-options "-O -Wstrict-aliasing -fstrict-aliasing" } */

extern int flag;

int foo() {

  int x;
  int y = 9;
  float* q;
  float* r;

  if (flag) {
    q = (float*) &x;  /* { dg-warning "type-punn" } */
  } else {
    q = (float*) &y;  /* { dg-warning "type-punn" } */
  }

  *q = 1.0;

  return x;

}
