#include <stdint.h>

void *
foo (int x)
{
  return __builtin_calloc (x * 1.1, 1); /* { dg-line calloc } */

  /* { dg-warning "use of floating-point arithmetic here might yield unexpected results" "warning" { target *-*-* } calloc } */
  /* { dg-message "operand '\(\\d|e|f|\\.|\\+|\)+' is of type 'double'" "note" { target *-*-* } calloc } */
  /* { dg-message "only use operands of an integer type inside the size argument" "note" { target *-*-* } calloc } */
}
