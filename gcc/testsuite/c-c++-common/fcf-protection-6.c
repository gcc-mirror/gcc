/* { dg-do compile } */
/* { dg-options "-fcf-protection=branch" } */
/* { dg-additional-options "-mshstk" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-error "'-fcf-protection=branch' requires Intel CET.*-mcet or -mibt option" "" { target { "i?86-*-* x86_64-*-*" } } 0 } */
/* { dg-error "'-fcf-protection=branch' is not supported for this target" "" { target { ! "i?86-*-* x86_64-*-*" } } 0 } */
