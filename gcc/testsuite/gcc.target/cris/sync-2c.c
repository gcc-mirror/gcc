/* Check that we don't get alignment-checking code, char.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tbreak\[ \t\]" } } */
/* { dg-final { scan-assembler-not "\tbtstq\[ \t\]\[^5\]" } } */
/* { dg-final { scan-assembler-not "\tand" } } */
/* { dg-final { scan-assembler-not "\t\[jb\]sr" } } */
#include "sync-1.c"
