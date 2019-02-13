/* Test redeclaration in an inner scope, with an incomplete type, of a
   file-scope initialized array shadowed in an intermediate scope (bug
   88584).  */
/* { dg-do compile } */
/* { dg-options "" } */

int a[1] = { 0 };

void
f (void)
{
  int a;
  {
    extern int a[];
    sizeof (a); /* { dg-error "incomplete" } */
  }
}
