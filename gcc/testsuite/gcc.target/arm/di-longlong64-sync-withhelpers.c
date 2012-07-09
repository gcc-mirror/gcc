/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v5_ok } */
/* { dg-options "-std=gnu99" } */
/* { dg-add-options arm_arch_v5 } */
/* { dg-message "note: '__sync_fetch_and_nand' changed semantics in GCC 4.4" "fetch_and_nand" { target *-*-* } 0 } */
/* { dg-message "note: '__sync_nand_and_fetch' changed semantics in GCC 4.4" "nand_and_fetch" { target *-*-* } 0 } */
/* { dg-message "file included" "In file included" { target *-*-* } 0 } */

#include "../../gcc.dg/di-longlong64-sync-1.c"

/* On an old ARM we have no ldrexd or strexd so we have to use helpers.  */
/* { dg-final { scan-assembler-not "ldrexd" } } */
/* { dg-final { scan-assembler-not "strexd" } } */
/* { dg-final { scan-assembler "__sync_" } } */
