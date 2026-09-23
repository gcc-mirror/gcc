/* { dg-do compile } */
/* { dg-options "-mptwrite -masm=intel" } */
/* { dg-skip-if "no intel asm" *-*-darwin* "" "" } */
void foo() { __builtin_ia32_ptwrite32(0); }
