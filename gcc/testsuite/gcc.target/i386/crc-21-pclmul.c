/* { dg-do run { target { pclmul_runtime } } } */
/* { dg-options "-mpclmul -O2 -fdump-rtl-dfinish -fdump-tree-crc" } */

#include "../../gcc.dg/torture/crc-21.c"

/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
/* Check 32-bit data CRC-32 expands with pclmul */
/* { dg-final { scan-rtl-dump "pclmulqdq" "dfinish"} } */
