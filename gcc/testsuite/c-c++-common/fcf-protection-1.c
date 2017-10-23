/* { dg-do compile } */
/* { dg-options "-fcf-protection=full" } */
/* { dg-error "'-fcf-protection=full' requires CET support on this target" "" { target { "i?86-*-* x86_64-*-*" } } 0 } */
/* { dg-error "'-fcf-protection=full' is not supported for this target" "" { target { ! "i?86-*-* x86_64-*-*" } } 0 } */
