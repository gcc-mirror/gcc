/* PR target/105778 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tand\[^\n\r]*\(31\|63\|127\|255\)" } } */

unsigned int f1 (unsigned int x, unsigned long y) { y &= 31; return x << y; }
unsigned int f2 (unsigned int x, unsigned long y) { return x << (y & 31); }
unsigned int f3 (unsigned int x, unsigned long y) { y &= 31; return x >> y; }
unsigned int f4 (unsigned int x, unsigned long y) { return x >> (y & 31); }
int f5 (int x, unsigned long y) { y &= 31; return x >> y; }
int f6 (int x, unsigned long y) { return x >> (y & 31); }
unsigned long long f7 (unsigned long long x, unsigned long y) { y &= 63; return x << y; }
unsigned long long f8 (unsigned long long x, unsigned long y) { return x << (y & 63); }
unsigned long long f9 (unsigned long long x, unsigned long y) { y &= 63; return x >> y; }
unsigned long long f10 (unsigned long long x, unsigned long y) { return x >> (y & 63); }
long long f11 (long long x, unsigned long y) { y &= 63; return x >> y; }
long long f12 (long long x, unsigned long y) { return x >> (y & 63); }
#ifdef __SIZEOF_INT128__
unsigned __int128 f13 (unsigned __int128 x, unsigned long y) { y &= 127; return x << y; }
unsigned __int128 f14 (unsigned __int128 x, unsigned long y) { return x << (y & 127); }
unsigned __int128 f15 (unsigned __int128 x, unsigned long y) { y &= 127; return x >> y; }
unsigned __int128 f16 (unsigned __int128 x, unsigned long y) { return x >> (y & 127); }
__int128 f17 (__int128 x, unsigned long y) { y &= 127; return x >> y; }
__int128 f18 (__int128 x, unsigned long y) { return x >> (y & 127); }
#endif
unsigned int f19 (unsigned int x, unsigned long y) { y &= 63; return x << y; }
unsigned int f20 (unsigned int x, unsigned long y) { return x << (y & 63); }
unsigned int f21 (unsigned int x, unsigned long y) { y &= 63; return x >> y; }
unsigned int f22 (unsigned int x, unsigned long y) { return x >> (y & 63); }
int f23 (int x, unsigned long y) { y &= 63; return x >> y; }
int f24 (int x, unsigned long y) { return x >> (y & 63); }
unsigned long long f25 (unsigned long long x, unsigned long y) { y &= 127; return x << y; }
unsigned long long f26 (unsigned long long x, unsigned long y) { return x << (y & 127); }
unsigned long long f27 (unsigned long long x, unsigned long y) { y &= 127; return x >> y; }
unsigned long long f28 (unsigned long long x, unsigned long y) { return x >> (y & 127); }
long long f29 (long long x, unsigned long y) { y &= 127; return x >> y; }
long long f30 (long long x, unsigned long y) { return x >> (y & 127); }
#ifdef __SIZEOF_INT128__
unsigned __int128 f31 (unsigned __int128 x, unsigned long y) { y &= 255; return x << y; }
unsigned __int128 f32 (unsigned __int128 x, unsigned long y) { return x << (y & 255); }
unsigned __int128 f33 (unsigned __int128 x, unsigned long y) { y &= 255; return x >> y; }
unsigned __int128 f34 (unsigned __int128 x, unsigned long y) { return x >> (y & 255); }
__int128 f35 (__int128 x, unsigned long y) { y &= 255; return x >> y; }
__int128 f36 (__int128 x, unsigned long y) { return x >> (y & 255); }
#endif
