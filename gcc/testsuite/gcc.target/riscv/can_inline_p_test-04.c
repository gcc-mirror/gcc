/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-einline-details" } */

#include "riscv_vector.h"
int
__attribute__((target("arch=+v,+zvl256b")))
foo (){
   return __riscv_vsetvl_e8m8 (100);
}


int
__attribute__((target("arch=+v")))
bar(){
  return foo();
}

/* { dg-final { scan-tree-dump {not inlinable: bar/\d+ -> foo/\d+, target specific option mismatch} "einline" } } */
/* { dg-final { scan-tree-dump-not {\(inlined\)} "einline" } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-O1" "-Og" ""} } */
