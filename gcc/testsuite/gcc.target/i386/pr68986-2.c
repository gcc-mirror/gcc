/* { dg-do compile { target ia32 } } */
/* { dg-require-effective-target tls_native } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fPIC -mno-accumulate-outgoing-args -mpreferred-stack-boundary=2" } */

extern __thread int msgdata;
int
foo ()
{
  return msgdata;
}

/* { dg-final { scan-assembler "andl\[\\t \]*\\$-16,\[\\t \]*%esp" } } */
