/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2" } */

typedef unsigned __int128 uv1ti __attribute__ ((__vector_size__ (16)));

uv1ti extqi(unsigned char c) { return (uv1ti)(__int128)c; }
uv1ti exthi(unsigned short s) { return (uv1ti)(__int128)s; }
uv1ti extsi(unsigned int i) { return (uv1ti)(__int128)i; }
uv1ti extdi(unsigned long long l) { return (uv1ti)(__int128)l; }

uv1ti pextqi(unsigned char *pc) { return (uv1ti)(__int128)(*pc); }
uv1ti pexthi(unsigned short *ps) { return (uv1ti)(__int128)(*ps); }
uv1ti pextsi(unsigned int *pi) { return (uv1ti)(__int128)(*pi); }
uv1ti pextdi(unsigned long long *pl) { return (uv1ti)(__int128)(*pl); }

/* { dg-final { scan-assembler-not "%\[er\]sp" } } */
