/* Test for C99 designated initializers */
/* Origin: Jakub Jelinek <jakub@redhat.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

struct A {
  int B;
  short C[2];
};
int a[10] = { 10, [4] = 15 };			/* { dg-error "ISO C89 forbids specifying subobject to initialize" } */
struct A b = { .B = 2 };			/* { dg-error "ISO C89 forbids specifying subobject to initialize" } */
struct A c[] = { [3].C[1] = 1 };		/* { dg-error "ISO C89 forbids specifying subobject to initialize" } */
struct A d[] = { [4 ... 6].C[0 ... 1] = 2 };	/* { dg-error "(forbids specifying range of elements to initialize)|(ISO C89 forbids specifying subobject to initialize)" } */
int e[] = { [2] 2 };				/* { dg-error "use of designated initializer without" } */
struct A f = { C: { 0, 1 } };			/* { dg-error "use of designated initializer with " } */
int g;

void foo (int *);

void bar (void)
{
  int x[] = { g++, 2 };				/* { dg-error "is not computable at load time" } */

  foo (x);
}
