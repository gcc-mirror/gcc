/* Test that the compiler does not crash when void expressions are
   found inside conditional expressions.  PR c/43082.  */
/* { dg-do compile } */

void
foo (int x)
{
  if (x ? (void)(0) : (void)(1)) /* { dg-error "void value not ignored as it ought to be" } */
    ;
}
