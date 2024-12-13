/* { dg-do run { target riscv_zbkb } } */
/* { dg-options "-march=rv64gc_zbkb -fdump-tree-crc-details" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zbkb -fdump-tree-crc-details" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-Oz" "-Og" "-flto"} } */

#include "../../gcc.dg/torture/crc-22.c"

/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
