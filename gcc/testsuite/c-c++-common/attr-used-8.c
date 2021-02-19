/* { dg-do compile } */
/* { dg-skip-if "non-ELF target" { *-*-darwin* } } */
/* { dg-options "-Wall -O2" } */

int __attribute__((section(".data.foo"))) foo1 = 1;
int __attribute__((used,section(".data.foo"))) foo2 = 2;

/* { dg-final { scan-assembler ".data.foo,\"aw\"" { target R_flag_in_section } } } */
/* { dg-final { scan-assembler-not ".data.foo,\"awR\"" { target R_flag_in_section } } } */
