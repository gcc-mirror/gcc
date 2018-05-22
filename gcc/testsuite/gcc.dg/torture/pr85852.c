/* { dg-do compile } */
/* { dg-additional-options "-fexceptions -fnon-call-exceptions" } */
/* { dg-additional-options "-fexceptions -fnon-call-exceptions -mfma" { target i?86-*-* x86_64-*-* } } */

void
ki (double nq)
{
  double no = 1.1 * nq - nq;
}
