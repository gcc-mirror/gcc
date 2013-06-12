/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int A[10], B[10][10];
int foo (int a);

int main () {
  A[:] = foo (B[:][:]); /* { dg-error "rank mismatch between" } */
  A[0] = foo (B[:][:]); /* { dg-error "cannot be scalar when" } */
  return 0;
}
