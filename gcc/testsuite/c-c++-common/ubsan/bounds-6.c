/* { dg-do run } */
/* { dg-options "-fsanitize=bounds -Wall -Wextra -Wno-array-bounds" } */

/* Test off-by-one.  */

struct S { int a; int b; } s[4], *t;
struct U { int a[10]; } u[4], *v;
volatile int *a, *b, *c;
volatile void *d;
volatile int e[4][4];

int
main (void)
{
  t = &s[4];  // OK
  a = &s[4].a; // Error
  b = &s[4].b; // Error
  d = &e[4];  // OK
  c = &e[4][0]; // Error
  c = &e[3][4]; // OK
  c = &e[3][3]; // OK

  a = &u[4].a[9]; // Error
  a = &u[4].a[10]; // Error
  a = &u[3].a[9]; // OK
  a = &u[3].a[10]; // OK
  a = &u[3].a[11]; // Error, warns with -Warray-bounds, but only if VRP runs

  return 0;
}

/* { dg-output "index 4 out of bounds for type 'S \\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 4 out of bounds for type 'S \\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 4 out of bounds for type 'int \\\[4\\\]\\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 4 out of bounds for type 'U \\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 4 out of bounds for type 'U \\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 11 out of bounds for type 'int \\\[10\\\]'" } */
