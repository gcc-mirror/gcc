/* PR c/123500 */
/* { dg-do compile } */
/* { dg-options "-Wbad-function-cast" } */

#include <stdint.h>
struct buffer {
  uint8_t * ptr __attribute__((counted_by(len)));
  int len;
};

uintptr_t foo(struct buffer * b) {
  return (uintptr_t)b->ptr;
}
