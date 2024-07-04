/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* Check that, along with a strub pure function call, we issue an asm statement
   to make sure the watermark passed to it is not assumed to be unchanged.  */

int __attribute__ ((__strub__, __pure__))
f() {
  static int i; /* Stop it from being detected as const.  */
  return i;
}

int
g() {
  return f();
}

/* { dg-final { scan-ipa-dump-times "__asm__" 1 "strub" } } */
