/* PR target/102986 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2" } */

typedef unsigned __int128 uv1ti __attribute__ ((__vector_size__ (16)));
typedef __int128 sv1ti __attribute__ ((__vector_size__ (16)));

uv1ti ashl(uv1ti x, unsigned int i) { return x << i; }
uv1ti lshr(uv1ti x, unsigned int i) { return x >> i; }
sv1ti ashr(sv1ti x, unsigned int i) { return x >> i; }
uv1ti rotr(uv1ti x, unsigned int i) { return (x >> i) | (x << (128-i)); }
uv1ti rotl(uv1ti x, unsigned int i) { return (x << i) | (x >> (128-i)); }

