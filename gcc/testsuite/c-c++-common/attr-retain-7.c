/* { dg-do compile { target R_flag_in_section } } */
/* { dg-skip-if "non-ELF target" { *-*-darwin* powerpc*-*-aix* } } */
/* { dg-options "-Wall -O2" } */

int __attribute__((used,retain,section(".data.foo"))) foo2 = 2;
int __attribute__((section(".data.foo"))) foo1 = 1;
/* { dg-warning "'.*' without 'retain' attribute and '.*' with 'retain' attribute are placed in a section with the same name" "" { target R_flag_in_section } .-1 } */

/* { dg-final { scan-assembler ".data.foo,\"aw\"" { target R_flag_in_section } } } */
/* { dg-final { scan-assembler ".data.foo,\"awR\"" { target R_flag_in_section } } } */
