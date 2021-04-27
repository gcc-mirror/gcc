/* PR target/99405 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic -fomit-frame-pointer" } */
/* { dg-final { scan-assembler-not "\tand\[bl]\t\\\$" } } */

unsigned char f1 (unsigned char x, unsigned y) { return (x << (y & 7)) | (x >> (-y & 7)); }
unsigned short f2 (unsigned short x, unsigned y) { return (x << (y & 15)) | (x >> (-y & 15)); }
unsigned int f3 (unsigned int x, unsigned y) { return (x << (y & 31)) | (x >> (-y & 31)); }
unsigned char f4 (unsigned char x, unsigned y) { return (x >> (y & 7)) | (x << (-y & 7)); }
unsigned short f5 (unsigned short x, unsigned y) { return (x >> (y & 15)) | (x << (-y & 15)); }
unsigned int f6 (unsigned int x, unsigned y) { return (x >> (y & 31)) | (x << (-y & 31)); }
unsigned char f7 (unsigned char x, unsigned char y) { unsigned char v = y & 7; unsigned char w = -y & 7; return (x << v) | (x >> w); }
unsigned short f8 (unsigned short x, unsigned char y) { unsigned char v = y & 15; unsigned char w = -y & 15; return (x << v) | (x >> w); }
unsigned int f9 (unsigned int x, unsigned char y) { unsigned char v = y & 31; unsigned char w = -y & 31; return (x << v) | (x >> w); }
unsigned char f10 (unsigned char x, unsigned char y) { unsigned char v = y & 7; unsigned char w = -y & 7; return (x >> v) | (x << w); }
unsigned short f11 (unsigned short x, unsigned char y) { unsigned char v = y & 15; unsigned char w = -y & 15; return (x >> v) | (x << w); }
unsigned int f12 (unsigned int x, unsigned char y) { unsigned char v = y & 31; unsigned char w = -y & 31; return (x >> v) | (x << w); }
#ifdef __x86_64__
unsigned long long f13 (unsigned long long x, unsigned y) { return (x << (y & 63)) | (x >> (-y & 63)); }
unsigned long long f14 (unsigned long long x, unsigned y) { return (x >> (y & 63)) | (x << (-y & 63)); }
unsigned long long f15 (unsigned long long x, unsigned char y) { unsigned char v = y & 63; unsigned char w = -y & 63; return (x << v) | (x >> w); }
unsigned long long f16 (unsigned long long x, unsigned char y) { unsigned char v = y & 63; unsigned char w = -y & 63; return (x >> v) | (x << w); }
#endif
