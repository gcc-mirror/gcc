/* Test for C99 designated initializer warnings and errors */
/* Origin: Jakub Jelinek <jakub@redhat.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -Wall -pedantic-errors" } */

typedef struct {
  int B;
  short C[2];
} A;
A a = { [2] = 1 };			/* { dg-error "(array index in non-array)|(near initialization)" } */
int b[] = { .B = 1 };			/* { dg-error "(field name not in record)|(near initialization)" } */
A c[] = { [0].D = 1 };			/* { dg-error "unknown field" } */
int d;
int e = { d++ };			/* { dg-error "(is not constant)|(near initialization)" } */
A f[2] = { [0].C[0] = 1, [2] = { 2, { 1, 2 } } };/* { dg-error "(array index in initializer exceeds array bounds)|(near initialization)" } */
int g[4] = { [1] = 1, 2, [6] = 5 };	/* { dg-error "(array index in initializer exceeds array bounds)|(near initialization)" } */
int h[] = { [0 ... 3] = 5 };		/* { dg-error "forbids specifying range of elements" } */
int i[] = { [2] 4 };			/* { dg-error "use of designated initializer without" } */
A j = { B: 2 };				/* { dg-error "use of designated initializer with " } */

void foo (int *, A *);

void bar (void)
{
  int a[] = { d++, [0] = 1 };		/* { dg-warning "(initialized field with side-effects overwritten)|(near initialization)" } */
  A b = { 1, { d++, 2 }, .C[0] = 3 };/* { dg-warning "(initialized field with side-effects overwritten)|(near initialization)" } */
  A c = { d++, { 2, 3 }, .B = 4 };	/* { dg-warning "(initialized field with side-effects overwritten)|(near initialization)" } */

  foo (a, d ? &b : &c);
}
