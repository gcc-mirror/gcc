/* { dg-do run { target { rv64 && riscv_zbkc_ok } } } */
/* { dg-options "-march=rv64gc_zbkc -fdump-tree-crc -fdump-rtl-dfinish" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */

#include "../../gcc.dg/torture/crc-21.c"

/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
/* { dg-final { scan-rtl-dump "clmul" "dfinish"} } */
