/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-ifunc "" }  */

int fum (); // Extra declaration that is merged with the second one.
int fum () __attribute__ ((target("default")));


int fum () __attribute__((target( "mmx")));
int fum () __attribute__((target( "popcnt")));
int fum () __attribute__((target( "sse")));
int fum () __attribute__((target( "sse2")));
int fum () __attribute__((target( "sse3")));
int fum () __attribute__((target( "ssse3")));
int fum () __attribute__((target( "sse4.1")));
int fum () __attribute__((target( "sse4.2")));
int fum () __attribute__((target( "avx")));
int fum () __attribute__((target( "avx2")));

int fum () __attribute__((target("arch=core2")));
int fum () __attribute__((target("arch=corei7")));
int fum () __attribute__((target("arch=atom")));

int (*p)() = &fum;

int j = fum();
