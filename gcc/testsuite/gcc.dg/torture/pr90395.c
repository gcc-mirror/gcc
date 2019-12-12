/* { dg-do compile } */
/* { dg-additional-options "-fexceptions -fnon-call-exceptions" } */

typedef int v16si __attribute__ ((__vector_size__ (64)));

void
rl (int uq)
{
  v16si qw[1];

  qw[uq] = (v16si) { uq };
}
