/* { dg-do compile { target { powerpc-*-* powerpcle-*-* } } } */
/* { dg-options "-mcpu=603e -fsched-stalled-insns -fsched2-use-superblocks -fschedule-insns2 -fno-dce -fno-guess-branch-probability --param max-cse-insns=4" } */

#include "../../gcc.target/powerpc/ppc-switch-2.c"
