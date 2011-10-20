/* { dg-do compile } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-options "-marm -std=gnu99" } */
/* { dg-require-effective-target arm_arch_v6k_ok } */
/* { dg-add-options arm_arch_v6k } */
/* { dg-message "note: '__sync_fetch_and_nand' changed semantics in GCC 4.4" "" { target *-*-* } 0 } */
/* { dg-message "note: '__sync_nand_and_fetch' changed semantics in GCC 4.4" "" { target *-*-* } 0 } */
/* { dg-message "file included" "In file included" { target *-*-* } 0 } */

#include "../../gcc.dg/di-longlong64-sync-1.c"

/* We should be using ldrexd, strexd and no helpers or shorter ldrex.  */
/* { dg-final { scan-assembler-times "\tldrexd" 46 } } */
/* { dg-final { scan-assembler-times "\tstrexd" 46 } } */
/* { dg-final { scan-assembler-not "__sync_" } } */
/* { dg-final { scan-assembler-not "ldrex\t" } } */
/* { dg-final { scan-assembler-not "strex\t" } } */
