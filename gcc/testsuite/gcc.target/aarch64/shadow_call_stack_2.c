/* { dg-do compile } */
/* { dg-options "-fsanitize=shadow-call-stack -ffixed-x18 -fexceptions" } */
/* { dg-skip-if "conflicts with x18" { aarch64-*-vxworks* } } */

int i;

/* { dg-error "'-fsanitize=shadow-call-stack' requires '-fno-exceptions'" "" {target "aarch64*-*-*" } 0 } */
