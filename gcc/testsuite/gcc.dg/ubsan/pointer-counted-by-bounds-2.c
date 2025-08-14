/* Test the attribute counted_by for pointer fields and its usage in
   bounds sanitizer combined with VLA.  */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */
/* { dg-output "index 11 out of bounds for type 'int \\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 20 out of bounds for type 'int \\\[\\\*\\\]\\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 11 out of bounds for type 'int \\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 10 out of bounds for type 'int \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */


#include <stdlib.h>

void __attribute__((__noinline__)) setup_and_test_vla (const int n, int m)
{
  struct foo {
    int n;
    int (*p)[n] __attribute__((counted_by(n)));
  } *f;

  f = (struct foo *) malloc (sizeof (struct foo));
  f->p = (int (*)[n]) malloc (m * sizeof (int[n]));
  f->n = m;
  f->p[m][n-1] = 1;
  free (f->p);
  free (f);
  return;
}

void __attribute__((__noinline__)) setup_and_test_vla_1 (const int n1,
							 const int n2,
							 int m)
{
  struct foo {
    int n;
    int (*p)[n2][n1] __attribute__((counted_by(n)));
  } *f;

  f = (struct foo *) malloc (sizeof(struct foo));
  f->p = (int (*)[n2][n1]) malloc (m * sizeof (int[n2][n1]));
  f->n = m;
  f->p[m][n2][n1] = 1;
  free (f->p);
  free (f);
  return;
}

int main(int argc, char *argv[])
{
  setup_and_test_vla (10, 11);
  setup_and_test_vla_1 (10, 11, 20);
  return 0;
}

