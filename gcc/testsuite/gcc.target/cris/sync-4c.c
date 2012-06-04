/* Check that we get don't alignment-checking code, xchg variant, char.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-trap-unaligned-atomic" } */
/* { dg-final { scan-assembler-not "\tbreak\[ \t\]" } } */
/* { dg-final { scan-assembler-not "\tbtstq\[ \t\]\[^5\]" } } */
/* { dg-final { scan-assembler-not "\tand" } } */
/* { dg-final { scan-assembler-not "\t\[jb\]sr" } } */
#include "sync-1.c"
