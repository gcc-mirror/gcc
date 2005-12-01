/* Make sure that we honor initial-exec.  */
/* { dg-do compile { target alpha*-*-* } } */
/* { dg-options "" } */
/* { dg-require-effective-target tls } */

static __thread int xyzzy __attribute__ ((tls_model ("initial-exec")));
int foo(void) { return xyzzy; }

/* { dg-final { scan-assembler "gottprel" } } */
/* { dg-final { scan-assembler-not "tprel(lo|hi|16)" } } */
