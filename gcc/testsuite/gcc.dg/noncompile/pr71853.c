/* PR c/71853 */
/* { dg-do compile } */

void f (void)
{
  case (0) { /* { dg-error "expected" } */
    switch 0: { } /* { dg-error "expected" } */
  }
}
