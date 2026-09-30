/* PR middle-end/127492 */
/* { dg-do run { target { sync_long_long_runtime || libatomic_available } } } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-latomic" { target libatomic_available } } */

#define T long long
#include "pr127492.c"
