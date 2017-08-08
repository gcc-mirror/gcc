/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-all -mstack-protector-guard=tls -mstack-protector-guard-reg=gs -mstack-protector-guard-offset=0x3038" } */

void f(void) { }

/* { dg-final { scan-assembler "gs:12344" } } */
