/* { dg-do compile } */
/* { dg-options "-fcf-protection" } */
/* { dg-skip-if "" { "riscv*-*-*" } } */
/* { dg-error "'-fcf-protection=full' is not supported for this target" "" { target { ! "i?86-*-* x86_64-*-*" } } 0 } */
