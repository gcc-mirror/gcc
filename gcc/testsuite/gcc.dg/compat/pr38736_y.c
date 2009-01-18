/* PR target/38736 */
/* { dg-options "-O2 -mavx" } */

#define aligned_x aligned_y_avx

#include "pr38736_x.c"
