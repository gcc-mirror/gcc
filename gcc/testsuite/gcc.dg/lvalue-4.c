/* PR c/27721 */
/* { dg-do compile } */

void foo()
{
  int i();
  i += 0;  /* { dg-error "invalid lvalue" } */
}
