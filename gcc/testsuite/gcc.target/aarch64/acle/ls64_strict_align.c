/* { dg-do compile } */
/* { dg-options "-mstrict-align" } */
/* PR target/113657 */

#pragma GCC target "+ls64"
#pragma GCC aarch64 "arm_acle.h"
__arm_data512_t foo(__arm_data512_t* ptr) { return *ptr; }
