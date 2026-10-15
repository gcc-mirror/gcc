/* { dg-do run { target { pclmul_runtime } } } */
/* { dg-options "-mpclmul -O2 -fdump-rtl-dfinish -fdump-tree-crc -fdisable-tree-phiopt2 -fdisable-tree-phiopt3" } */

#include "../../gcc.dg/torture/crc-1.c"

/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
/* Check 16-bit data CRC-16 expands with pclmul */
/* { dg-final { scan-rtl-dump "pclmulqdq" "dfinish"} } */
