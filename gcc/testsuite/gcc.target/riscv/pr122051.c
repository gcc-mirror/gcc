/* { dg-do compile { target int128 } } */
/* { dg-additional-options "-mrvv-vector-bits=zvl -mcpu=xt-c920 -w" } */

typedef __attribute__((__vector_size__(4))) char B;
typedef __attribute__((__vector_size__(16))) long V;
typedef __attribute__((__vector_size__(32))) double W;
typedef __attribute__((__vector_size__(32))) char U;
unsigned u;
B o;
char *p;
int q;
V v;
W w;

void
foo(__int128, __int128, __int128, __int128, B a, B b, B c, B d, B e, B f, B g, B h) {
  do {
    w -= q;
    v ^= u;
  } while (__builtin_memcmp(p, 1 + p, 7));
  o = ((U)w)[0] + c + d + e + f + g + h + a + b;
}


