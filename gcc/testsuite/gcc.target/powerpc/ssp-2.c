/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-all -mstack-protector-guard=tls -mstack-protector-guard-reg=r18 -mstack-protector-guard-offset=0x3038" } */

/* { dg-final { scan-assembler {\m12344\(r?18\)} } } */

void f(void) { }
