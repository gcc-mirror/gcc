/* PR c/7411 */
/* Contributed by Christian Ehrhardt */
/* { dg-do compile } */

void foo(void)
{
  char c;  /* { dg-error "previous declaration" } */
  int i;
  int c = i;  /* { dg-error "conflicting types" } */
}
