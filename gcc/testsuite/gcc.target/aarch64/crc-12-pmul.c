/* { dg-do run } */
/* { dg-options "-march=armv8-a+crypto -O2 -fdump-rtl-dfinish -fdump-tree-crc -fdisable-tree-phiopt2 -fdisable-tree-phiopt3" } */
/* { dg-skip-if "" { *-*-* } { "-flto"} } */

#include "../../gcc.dg/torture/crc-12.c"

/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
/* { dg-final { scan-rtl-dump "pmull" "dfinish"} } */
