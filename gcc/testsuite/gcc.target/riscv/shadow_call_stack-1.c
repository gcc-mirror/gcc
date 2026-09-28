/* { dg-do compile { target { riscv*-*-* } } } */
/* { dg-options "-fsanitize=shadow-call-stack" } */

int i;

/* { dg-error "'-fsanitize=shadow-call-stack' software shadow call stack needs explicit '-mno-relax'" "" { target "riscv*-*-*" } 0 } */
