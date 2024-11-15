/* { dg-do compile } */
/* { dg-options "-fcf-protection=return" } */
/* { dg-skip-if "" { "riscv*-*-*" } } */
/* { dg-error "'-fcf-protection=return' is not supported for this target" "" { target { ! "i?86-*-* x86_64-*-*" } } 0 } */
