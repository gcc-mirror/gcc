/* PR target/81532 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -mavx512bw -mavx512vl -mno-avx512dq" } */

#include "avx512dq-pr81532.c"
