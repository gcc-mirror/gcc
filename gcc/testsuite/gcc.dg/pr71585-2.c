/* Test that stack protection is done on chosen functions. */

/* { dg-do compile { target i?86-*-* x86_64-*-* rs6000-*-* s390x-*-* } } */
/* { dg-options "-O2 -fstack-protector-all" } */

/* This test checks the presence of __stack_chk_fail function in assembler.
 * Compiler generates _stack_chk_fail_local (wrapper) calls instead for PIC.
 */
/* { dg-require-effective-target nonpic } */

static int foo()
{
  return 0;
}

#pragma GCC push_options
#pragma GCC optimize ("-fno-stack-protector")

int main() { foo (); }

#pragma GCC pop_options

/* { dg-final { scan-assembler-times "stack_chk_fail" 0 } } */
