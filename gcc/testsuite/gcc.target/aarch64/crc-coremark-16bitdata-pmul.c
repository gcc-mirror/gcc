/* { dg-do run } */
/* { dg-options "-w -march=armv8-a+crypto -O2 -fdump-rtl-dfinish -fdump-tree-crc" } */
/* { dg-skip-if "" { *-*-* } { "-flto"} } */

#include "../../gcc.dg/torture/crc-coremark16-data16.c"

/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
/* { dg-final { scan-rtl-dump "pmull" "dfinish"} } */