/* { dg-do run } */
/* { dg-require-effective-target sync_char_short } */
/* { dg-options "-O2" } */
/* { dg-options "-march=i486 -O2" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-mcpu=v9 -O2" { target sparc*-*-* } } */

/* Test functionality of the intrinsics for 'short' and 'char'.  */

#define AI_ALIGN __attribute__((__aligned__ (4)))
#include "sync-2.c"
