/* { dg-do run } */
/* { dg-additional-sources "../sync-1.c" } */
/* { dg-options "-Dop -Dtype=int -Dmisalignment=3 -DTRAP_USING_ABORT -mno-trap-using-break8" } */
/* { dg-additional-options "-mtrap-unaligned-atomic" { target cris-*-elf } } */
/* { dg-additional-options "-mno-unaligned-atomic-may-use-library" { target cris*-*-linux* } } */
#include "sync-mis-op-s-1.c"
