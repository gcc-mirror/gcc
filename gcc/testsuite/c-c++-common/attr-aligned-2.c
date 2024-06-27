/* PR c/115549 */
/* { dg-do compile } */

__attribute__((aligned,optimize(s))) /* { dg-error "not declared|undeclared" } */
int s()
{
  return 0;
}
