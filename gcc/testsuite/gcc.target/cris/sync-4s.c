/* Check that we don't get alignment-checking code, short.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Dtype=short -mno-trap-unaligned-atomic" } */
/* { dg-final { scan-assembler-not "\tbreak\[ \t\]" } } */
/* { dg-final { scan-assembler-not "\tbtstq\[ \t\]\[^5\]" } } */
/* { dg-final { scan-assembler-not "\tand" } } */
/* { dg-final { scan-assembler-not "\t\[jb\]sr" } } */
#include "sync-1.c"
