/* PR c/67730 */
/* { dg-do compile } */
/* { dg-options "" } */

#include <stddef.h>

void
fn1 (void)
{
  return NULL; /* { dg-error "10:.return. with a value" } */
}
