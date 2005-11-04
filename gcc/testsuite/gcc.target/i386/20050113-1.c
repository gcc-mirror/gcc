/* PR middle-end/19164 */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-mmmx" } */

typedef short int V __attribute__ ((vector_size (8)));
static V v = (V) 0x00FF00FF00FF00FFLL; /* { dg-error "is not constant" } */
