/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fPIC -mno-accumulate-outgoing-args -mpreferred-stack-boundary=5 -mincoming-stack-boundary=4" } */

extern __thread int msgdata;
int
foo ()
{
  return msgdata;
}
