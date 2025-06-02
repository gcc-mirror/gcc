/* { dg-do compile } */
/* { dg-options "-fsanitize=shadow-call-stack -fno-exceptions" } */

int i;

/* aarch64-*-vxworks has x18 as a fixed register.  */
/* { dg-error "'-fsanitize=shadow-call-stack' requires '-ffixed-x18'" "" { target { aarch64*-*-* && { ! aarch64-*-vxworks* } } } 0 } */
/* { dg-message "sorry, unimplemented: '-fsanitize=shadow-call-stack' conflicts with the use of register x18" "" { target { aarch64-*-vxworks* } } 0 } */
