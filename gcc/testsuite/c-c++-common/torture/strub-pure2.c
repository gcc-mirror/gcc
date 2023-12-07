/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* Check that, along with a strub implicitly-pure function call, we issue an asm
   statement to make sure the watermark passed to it is not assumed to be
   unchanged.  */

int __attribute__ ((__strub__))
#if ! __OPTIMIZE__ /* At -O0, implicit pure detection doesn't run.  */
__attribute__ ((__pure__))
#endif
f() {
  static int i; /* Stop it from being detected as const.  */
  return i;
}

int
g() {
  return f();
}

/* { dg-final { scan-ipa-dump-times "__asm__" 1 "strub" } } */
