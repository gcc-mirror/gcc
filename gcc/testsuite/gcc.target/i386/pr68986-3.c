/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fPIC -mno-sse -mpreferred-stack-boundary=3 -mincoming-stack-boundary=3" } */

extern __thread int msgdata;
int
foo ()
{
  return msgdata;
}

/* { dg-final { scan-assembler "and\[lq\]\[\\t \]*\\$-16,\[\\t \]*%\[re\]?sp" } } */
