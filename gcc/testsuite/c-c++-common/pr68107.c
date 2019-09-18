/* PR c/68107 */
/* { dg-do compile } */

#define N ((__SIZE_MAX__ / sizeof (int)) / 2 + 1)

typedef int (*T1)[N]; /* { dg-error "15:exceeds maximum object size" } */
typedef int (*T2)[N - 1];
typedef int (*T3)[N][N]; /* { dg-error "15:exceeds maximum object size" } */
typedef int (*T4)[N - 1][N - 1]; /* { dg-error "15:exceeds maximum object size" } */
typedef int (**T5)[N]; /* { dg-error "16:exceeds maximum object size" } */

struct S {
  int (*q1)[N]; /* { dg-error "9:exceeds maximum object size" } */
  int (*q2)[N - 1];
  int (*q3)[N][N]; /* { dg-error "9:exceeds maximum object size" } */
  int (*q4)[N - 1][N - 1]; /* { dg-error "9:exceeds maximum object size" } */
  int (**q5)[N]; /* { dg-error "10:exceeds maximum object size" } */
};

void fn1 (int (*p1)[N]); /* { dg-error "17:exceeds maximum object size" } */
void fn2 (int (*p1)[N - 1]);
void fn3 (int (*p3)[N][N]); /* { dg-error "17:exceeds maximum object size" } */
void fn4 (int (*p4)[N - 1][N - 1]); /* { dg-error "17:exceeds maximum object size" } */
void fn5 (int (**p5)[N]); /* { dg-error "18:exceeds maximum object size" } */

void
fn (void)
{
  int (*n1)[N]; /* { dg-error "9:exceeds maximum object size" } */
  int (*n2)[N - 1];
  int (*n3)[N][N]; /* { dg-error "9:exceeds maximum object size" } */
  int (*n4)[N - 1][N - 1]; /* { dg-error "9:exceeds maximum object size" } */
  int (**n5)[N]; /* { dg-error "10:exceeds maximum object size" } */

  sizeof (int (*)[N]); /* { dg-error "exceeds maximum object size" } */
  sizeof (int [N]); /* { dg-error "exceeds maximum object size" } */
}
