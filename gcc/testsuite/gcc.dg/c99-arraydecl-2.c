/* Test for C99 array declarators: expression must be an
   assignment-expression.  PR 11943.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

void
foo (void)
{
  int a[2, 3]; /* { dg-error "parse|syntax|expected" "bad array declarator" } */
  void b(int x[2, 3]); /* { dg-error "parse|syntax|expected" "bad array declarator" } */
  void c(int [2, 3]); /* { dg-error "parse|syntax|expected" "bad array declarator" } */
  void d(int *x[restrict 2, 3]); /* { dg-error "parse|syntax|expected" "bad array declarator" } */
  void e(int *x[static restrict 2, 3]); /* { dg-error "parse|syntax|expected" "bad array declarator" } */
  void f(int *x[restrict static 2, 3]); /* { dg-error "parse|syntax|expected" "bad array declarator" } */
}
