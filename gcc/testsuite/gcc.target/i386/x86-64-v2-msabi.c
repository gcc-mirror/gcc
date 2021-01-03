/* { dg-do compile { target lp64 } } */
/* { dg-options "-mabi=ms -march=x86-64-v2" } */

/* Verify -march=x86-64-v2 works even with -mabi=ms.  */
#include "x86-64-v2.c"
