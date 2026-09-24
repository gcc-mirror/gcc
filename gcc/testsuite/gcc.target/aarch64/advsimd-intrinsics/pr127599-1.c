/* { dg-do compile } */
/* { dg-additional-options "-O1" } */
/* PR target/127599 */

#include <arm_neon.h>

int32x4_t b;
void c() {
  b = vrev64q_s32(b);
  b = vextq_s32(b, b, 2);
}
