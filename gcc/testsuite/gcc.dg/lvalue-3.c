/* Test that assignment of a read-only variable that gets const-ness
   from a read-only field is diagnosed.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

struct s { const int a; } x;
typeof (x.a) b;
void
f (void)
{
  x.a = 1; /* { dg-error "error: assignment of read-only member 'a'" } */
  b = 1; /* { dg-error "error: assignment of read-only variable 'b'" } */
}
