/* PR30949 */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

int func (int x);
void recv (int (* funcptr) (double x)); /* { dg-message "note: expected 'int .\\\*..double.' but argument is of type 'int .\\\*..int.'" } */
void call (void)
{
  recv (func); /* { dg-error "passing argument 1 of 'recv' from incompatible pointer type" } */
}

