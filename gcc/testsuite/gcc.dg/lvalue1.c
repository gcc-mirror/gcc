/* PR c/5225 */
/* { dg-do compile } */

int main()
{
  int i;
  +i = 1;	/* { dg-error "lvalue required as left operand of assignment" } */
  return 0;
}
