/* { dg-do compile } */
/* { dg-options "-fcf-protection=return" } */
/* { dg-additional-options "-mibt" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-error "'-fcf-protection=return' requires Intel CET.*-mcet or -mshstk option" "" { target { "i?86-*-* x86_64-*-*" } } 0 } */
/* { dg-error "'-fcf-protection=return' is not supported for this target" "" { target { ! "i?86-*-* x86_64-*-*" } } 0 } */
