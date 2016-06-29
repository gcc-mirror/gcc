/* Test that stack protection is done on chosen functions. */

/* { dg-do compile { target i?86-*-* x86_64-*-* rs6000-*-* s390x-*-* } } */
/* { dg-options "-O2 -fstack-protector-all" } */

/* This test checks the presence of __stack_chk_fail function in assembler.
 * Compiler generates _stack_chk_fail_local (wrapper) calls instead for PIC.
 */
/* { dg-require-effective-target nonpic } */

#pragma GCC push_options

#pragma GCC optimize ("-fno-stack-protector")
__attribute__((constructor)) void foo()
{
  asm ("");
}

#pragma GCC pop_options

int main() { return 0; }

/* { dg-final { scan-assembler-times "stack_chk_fail" 1 } } */
