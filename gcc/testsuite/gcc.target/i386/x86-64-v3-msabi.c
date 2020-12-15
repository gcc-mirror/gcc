/* { dg-do compile { target lp64 } } */
/* { dg-options "-mabi=ms -march=x86-64-v3" } */

/* Verify -march=x86-64-v3 works even with -mabi=ms.  */
#include "x86-64-v3.c"
