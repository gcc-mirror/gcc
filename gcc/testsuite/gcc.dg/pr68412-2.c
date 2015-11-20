/* PR c/68412 */
/* { dg-do compile } */
/* { dg-options "-Wall -Wextra" } */

int
fn1 (int i)
{
  return ({ i; }) == ({ i; }); /* { dg-warning "self-comparison always evaluates to true" } */
}

int
fn2 (int i)
{
  return ({ i + 1; }) != ({ i + 1; }); /* { dg-warning "self-comparison always evaluates to false" } */
}
