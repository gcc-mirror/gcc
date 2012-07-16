/* Check that we get the expected alignment-checking code, xchg variant, short.
   Unfortunately, PRE moves the "and" to a different BB, so combine doesn't
   see it with the compare to make it a btstq.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Dxchg -Dtype=short" } */
/* { dg-additional-options "-mtrap-using-break8 -mtrap-unaligned-atomic" { target cris-*-elf } } */
/* { dg-additional-options "-mno-unaligned-atomic-may-use-library" { target cris*-*-linux* } } */
/* { dg-final { scan-assembler "\tbreak 8" } } */
/* { dg-final { scan-assembler "\tbtstq \\(1-1\\)," { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not "\tand" { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not "\t\[jb\]sr" } } */
#include "sync-1.c"
