/* PR c/68107 */
/* { dg-do compile } */

#define N ((__SIZE_MAX__ / sizeof (int)) / 2 + 1)

typedef int (*T1)[N]; /* { dg-error "too large" } */
typedef int (*T2)[N - 1];
typedef int (*T3)[N][N]; /* { dg-error "too large" } */
typedef int (*T4)[N - 1][N - 1]; /* { dg-error "too large" } */
typedef int (**T5)[N]; /* { dg-error "too large" } */

struct S {
  int (*q1)[N]; /* { dg-error "too large" } */
  int (*q2)[N - 1];
  int (*q3)[N][N]; /* { dg-error "too large" } */
  int (*q4)[N - 1][N - 1]; /* { dg-error "too large" } */
  int (**q5)[N]; /* { dg-error "too large" } */
};

void fn1 (int (*p1)[N]); /* { dg-error "too large" } */
void fn2 (int (*p1)[N - 1]);
void fn3 (int (*p3)[N][N]); /* { dg-error "too large" } */
void fn4 (int (*p4)[N - 1][N - 1]); /* { dg-error "too large" } */
void fn5 (int (**p5)[N]); /* { dg-error "too large" } */

void
fn (void)
{
  int (*n1)[N]; /* { dg-error "too large" } */
  int (*n2)[N - 1];
  int (*n3)[N][N]; /* { dg-error "too large" } */
  int (*n4)[N - 1][N - 1]; /* { dg-error "too large" } */
  int (**n5)[N]; /* { dg-error "too large" } */

  sizeof (int (*)[N]); /* { dg-error "too large" } */
  sizeof (int [N]); /* { dg-error "too large" } */
}
