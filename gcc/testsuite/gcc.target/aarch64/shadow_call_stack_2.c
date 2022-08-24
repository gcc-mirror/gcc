/* { dg-do compile } */
/* { dg-options "-fsanitize=shadow-call-stack -ffixed-x18 -fexceptions" } */

int i;

/* { dg-error "'-fsanitize=shadow-call-stack' requires '-fno-exceptions'" "" {target "aarch64*-*-*" } 0 } */
