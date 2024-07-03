/* { dg-do compile } */
/* Verify that subword atomics do not generate calls.  */
/* { dg-options "-minline-atomics" } */
/* { dg-add-options riscv_a } */
/* { dg-remove-options riscv_zabha } */
/* { dg-message "note: '__sync_fetch_and_nand' changed semantics in GCC 4.4" "fetch_and_nand" { target *-*-* } 0 } */
/* { dg-final { scan-assembler-not "\tcall\t__sync_fetch_and_add_1" } } */
/* { dg-final { scan-assembler-not "\tcall\t__sync_fetch_and_nand_1" } } */
/* { dg-final { scan-assembler-not "\tcall\t__sync_bool_compare_and_swap_1" } } */

#include "inline-atomics-1.c"
