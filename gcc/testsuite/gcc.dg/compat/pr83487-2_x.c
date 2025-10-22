/* { dg-options "-fno-common" { target { hppa*-*-hpux* } } } */
/* { dg-options "-Wno-psabi" { target { riscv*-*-* } } } */
#define PR83487_LARGE
#include "pr83487-1_x.c"
