/* { dg-do run } */
/* { dg-additional-sources "../sync-1.c" } */
/* { dg-options "-Dxchg -Dtype=int -DTRAP_USING_ABORT -mno-trap-using-break8" } */
/* { dg-additional-options "-mtrap-unaligned-atomic" { target cris-*-elf } } */
#include "sync-mis-op-s-1.c"
