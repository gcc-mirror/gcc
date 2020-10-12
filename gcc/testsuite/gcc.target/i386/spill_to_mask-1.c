/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512" } */

#ifndef DTYPE
#define DTYPE u32
#endif

typedef unsigned long long u64;
typedef unsigned int u32;
typedef unsigned short u16;
typedef unsigned char u8;

#define R(x,n) ( (x >> n) | (x << (32 - n)))

#define S0(x) (R(x, 2) ^ R(x,13) ^ R(x,22))
#define S1(x) (R(x, 6) ^ R(x,11) ^ R(x,25))

#define TT(a,b,c,d,e,f,g,h,x,K)                 \
{                                                        \
    tmp1 = h + S1(e) + (g ^ (e & (f ^ g))) + K + x;                \
    tmp2 = S0(a) + ((a & b) | (c & (a | b)));                           \
    h  = tmp1 + tmp2;                                    \
    d += tmp1;                                           \
}

static inline DTYPE byteswap(DTYPE x)
{
	x = (x & 0x0000FFFF) << 16 | (x & 0xFFFF0000) >> 16;
	x = (x & 0x00FF00FF) << 8 | (x & 0xFF00FF00) >> 8;  
	return x;
}

#define BE_LOAD32(n,b,i) (n) = byteswap(*(DTYPE *)(b + i))

void foo (u8 *in, DTYPE out[8], const DTYPE C[16])
{
    DTYPE tmp1 = 0, tmp2 = 0, a, b, c, d, e, f, g, h;
    DTYPE w0, w1, w2, w3, w4, w5, w6, w7,
	w8, w9, w10, w11, w12, w13, w14, w15;
    w0  = byteswap(*(DTYPE *)(in + 0));
    w1  = byteswap(*(DTYPE *)(in + 4));
    w2  = byteswap(*(DTYPE *)(in + 8));
    w3  = byteswap(*(DTYPE *)(in + 12));
    w4  = byteswap(*(DTYPE *)(in + 16));
    w5  = byteswap(*(DTYPE *)(in + 20));
    w6  = byteswap(*(DTYPE *)(in + 24));
    w7  = byteswap(*(DTYPE *)(in + 28));
    w8  = byteswap(*(DTYPE *)(in + 32));
    w9  = byteswap(*(DTYPE *)(in + 36));
    w10 = byteswap(*(DTYPE *)(in + 40));
    w11 = byteswap(*(DTYPE *)(in + 44));
    w12 = byteswap(*(DTYPE *)(in + 48));
    w13 = byteswap(*(DTYPE *)(in + 52));
    w14 = byteswap(*(DTYPE *)(in + 56));
    w15 = byteswap(*(DTYPE *)(in + 60));
    a = out[0];
    b = out[1];
    c = out[2];
    d = out[3];
    e = out[4];
    f = out[5];
    g = out[6];
    h = out[7];

    TT(a, b, c, d, e, f, g, h,  w0, C[0]);
    TT(h, a, b, c, d, e, f, g,  w1, C[1]);
    TT(g, h, a, b, c, d, e, f,  w2, C[2]);
    TT(f, g, h, a, b, c, d, e,  w3, C[3]);
    TT(e, f, g, h, a, b, c, d,  w4, C[4]);
    TT(d, e, f, g, h, a, b, c,  w5, C[5]);
    TT(c, d, e, f, g, h, a, b,  w6, C[6]);
    TT(b, c, d, e, f, g, h, a,  w7, C[7]);
    TT(a, b, c, d, e, f, g, h,  w8, C[8]);
    TT(h, a, b, c, d, e, f, g,  w9, C[9]);
    TT(g, h, a, b, c, d, e, f, w10, C[10]);
    TT(f, g, h, a, b, c, d, e, w11, C[11]);
    TT(e, f, g, h, a, b, c, d, w12, C[12]);
    TT(d, e, f, g, h, a, b, c, w13, C[13]);
    TT(c, d, e, f, g, h, a, b, w14, C[14]);
    TT(b, c, d, e, f, g, h, a, w15, C[15]);

    out[0] += a;
    out[1] += b;
    out[2] += c;
    out[3] += d;
    out[4] += e;
    out[5] += f;
    out[6] += g;
    out[7] += h;
}

/* { dg-final { scan-assembler "kmovd" } } */
