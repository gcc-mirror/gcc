/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-all -mstack-protector-guard=tls -mstack-protector-guard-reg=gs -mstack-protector-guard-symbol=my_guard" } */

void f(void) { }

/* { dg-final { scan-assembler "gs:my_guard" } } */
