/* { dg-do compile } */
/* { dg-options "-fcf-protection" } */
/* { dg-error "'-fcf-protection=full' requires Intel CET.*-mcet.*-mibt and -mshstk option" "" { target { "i?86-*-* x86_64-*-*" } } 0 } */
/* { dg-error "'-fcf-protection=full' is not supported for this target" "" { target { ! "i?86-*-* x86_64-*-*" } } 0 } */
