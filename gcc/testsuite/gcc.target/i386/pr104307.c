/* PR middle-end/104307 */
/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -mavx512f -fcompare-debug " } */

#include "pr78669.c"
