/* PR c/5225 */
/* { dg-do compile } */

int main()
{
  int i;
  +i = 1;	/* { dg-error "invalid lvalue in assignment" } */
  return 0;
}
