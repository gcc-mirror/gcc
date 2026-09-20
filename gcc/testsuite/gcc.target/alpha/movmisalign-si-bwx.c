/* Same as movmisalign-si.c, but for a BWX target, where the copy used to be
   made one byte at a time via a stack slot.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mno-safe-partial -mbwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#include "movmisalign-si.c"
