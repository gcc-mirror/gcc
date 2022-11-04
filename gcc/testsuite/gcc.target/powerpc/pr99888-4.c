/* { dg-require-effective-target powerpc_elfv2 } */
/* There is no global entry point prologue with pcrel.  */
/* { dg-options "-mno-pcrel -fpatchable-function-entry=1,1" } */

/* Verify one error emitted for unexpected 1 nop before local
   entry.  */

extern int a;

int test (int b) {
  return a + b;
}
/* { dg-error "unsupported number of nops before function entry \\(1\\)" "" { target *-*-* } .-1 } */
