/* { dg-do compile } */
/* { dg-options "-fsanitize=shadow-call-stack -fno-exceptions" } */

int i;

/* { dg-error "'-fsanitize=shadow-call-stack' requires '-ffixed-x18'" "" {target "aarch64*-*-*" } 0 } */
