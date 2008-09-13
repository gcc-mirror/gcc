/* { dg-do compile } */
/* { dg-options "-mavx" { target { i?86-*-* x86_64-*-* } } } */

/* Make sure that vector of size 8 of signed char works. This used to crash with AVX on x86
   as we would produce try to extract the chars inside the vector mode using the vector mode of V8SI
   which was wrong. */
__attribute__ ((vector_size (8))) signed char v4, v5, v6;
void
two (void)
{
 v4 = v5 + v6;
}

