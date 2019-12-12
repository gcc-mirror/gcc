/* PR c/89211 */
/* { dg-do compile } */

void foo ();
void foo ()
{
  void foo (struct S);	/* { dg-warning "declared inside parameter list" } */
}
