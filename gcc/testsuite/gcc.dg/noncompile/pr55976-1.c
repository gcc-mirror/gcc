/* PR c/55976 */
/* { dg-do compile } */
/* { dg-options "-Werror=return-type" } */

/* Verify warnings for return type become errors.  */

void t () { return 1; } /* { dg-error "return" "function returning void" } */
int b () { return; } /* { dg-error "return" "function returning non-void" } */

int main()
{
  t(); b();
  return 0;
}
