/* { dg-do compile } */
/* { dg-options "-O2" } */

int a;
void c(void);
int d(_Bool b) {
  switch (b+0) {
  case 0:
    break;
  case 1:
    break;
  default:
    c();
  }
  if (b)
    return a;
}
