/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-O2 -fstack-protector-all -mstack-protector-guard=tls -mstack-protector-guard-reg=gs -mstack-protector-guard-symbol=my_guard" } */
/* We don't expect GOT relocations; should we?  */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */

void f(void) { }

/* { dg-final { scan-assembler "gs:my_guard" } } */
