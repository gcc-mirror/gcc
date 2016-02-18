/* Test AAPCS layout

   Larger than machine-supported vector size.  The behavior is unspecified by
   the AAPCS64 document; the implementation opts for pass by reference.  */

/* { dg-do compile { target aarch64*-*-* } } */

typedef char A __attribute__ ((vector_size (64)));

void
foo (A a)
{
}
