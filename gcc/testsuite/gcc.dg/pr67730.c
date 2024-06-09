/* PR c/67730 */
/* { dg-do compile } */
/* { dg-options "-fpermissive" } */

#include <stddef.h>

void
fn1 (void)
{
  return NULL; /* { dg-warning "10:.return. with a value" } */
}
