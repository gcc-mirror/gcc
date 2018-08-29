/* PR target/80846 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -mavx512f" } */

#include "avx-pr80846.c"
