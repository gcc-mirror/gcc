/* { dg-do run { target { pclmul_runtime } } } */
/* { dg-options "-w -mpclmul -O2 -fdump-rtl-dfinish -fdump-tree-crc" } */
/* { dg-skip-if "" { *-*-* } { "-flto"} } */

#include "../../gcc.dg/torture/crc-coremark16-data16.c"

/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
/* Check 16-bit data CRC-16 expands with pclmul */
/* { dg-final { scan-rtl-dump "pclmulqdq" "dfinish"} } */
