/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-all -mstack-protector-guard=global" } */

/* { dg-final { scan-assembler "__stack_chk_guard" } } */

void f(void) { }
