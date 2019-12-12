/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-march=skylake -fPIC -fsched2-use-superblocks -fno-if-conversion" } */
/* { dg-require-effective-target fpic } */

#include "../../gcc.dg/20030309-1.c"
