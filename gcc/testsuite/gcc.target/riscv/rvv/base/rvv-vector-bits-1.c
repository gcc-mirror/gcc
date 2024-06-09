/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64 -mrvv-vector-bits=128 -O3" } */

#include "riscv_vector.h"

/* { dg-error "unrecognized argument in option '-mrvv-vector-bits=128'" "" { target { "riscv*-*-*" } } 0 } */
/* { dg-message "note: valid arguments to '-mrvv-vector-bits=' are: scalable zvl" "" { target { "riscv*-*-*" } } 0 } */
