/* PR c/62294 */
/* { dg-do compile } */

#include "pr62294.h"

void
fn (int *u)
{
  foo (u); /* { dg-error "passing argument 1 of .bar. from incompatible pointer type" } */
}
