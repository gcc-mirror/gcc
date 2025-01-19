/* Test ICE for shift with invalid redeclaration (bug 117757).  */
/* { dg-do compile } */
/* { dg-options "" } */

void
f (int a)
{
  1 << a;
  int a[1]; /* { dg-error "redeclared" } */
}
