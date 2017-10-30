/* { dg-do compile } */
/* { dg-options "-fcf-protection=return" } */
/* { dg-error "'-fcf-protection=return' requires CET support on this target" "" { target { "i?86-*-* x86_64-*-*" } } 0 } */
/* { dg-error "'-fcf-protection=return' is not supported for this target" "" { target { ! "i?86-*-* x86_64-*-*" } } 0 } */
