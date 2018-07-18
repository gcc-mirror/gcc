/* PR c/81289 */
/* { dg-do compile } */

int
fn (int mm)
{
  mm == *&&
} /* { dg-error "expected identifier" } */
