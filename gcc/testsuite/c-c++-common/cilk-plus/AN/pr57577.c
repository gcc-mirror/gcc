/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

#define NUMBER 100
int A[NUMBER], B[NUMBER][NUMBER];
int foo (int a);

int main () {
  A[:] = foo (B[:][:]); /* { dg-error "rank mismatch between" } */
  A[0] = foo (B[:][:]); /* { dg-error "cannot be scalar when" } */
  return 0;
}
