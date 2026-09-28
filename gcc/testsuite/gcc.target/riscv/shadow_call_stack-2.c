/* { dg-do compile { target { riscv*-*-* } } } */
/* { dg-options "-fsanitize=shadow-call-stack -mno-relax -fexceptions" } */

int i;
