/* PR c/7411 */
/* Contributed by Christian Ehrhardt */
/* Added extra line-breaks to check that diagnostics refer to correct token.
   --Per Bothner. */
/* { dg-do compile } */

void foo(void)
{
  char
    c	/* { dg-error "previous declaration" } */
    ;
  int i;
  int
    c	/* { dg-error "conflicting types" } */
    = i;
}
