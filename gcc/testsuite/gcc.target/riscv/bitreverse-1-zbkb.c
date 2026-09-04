/* { dg-do run { target riscv_zbkb } } */
/* { dg-options "-march=rv64gc_zbkb" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zbkb" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-Oz" "-Og" "-flto"} } */

#include "../../gcc.dg/builtin-bitreverse-1.c"
