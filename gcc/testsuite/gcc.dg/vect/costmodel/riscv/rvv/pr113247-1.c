/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=dynamic" } */

#include <stdint-gcc.h>

#define Ch(x,y,z)   (z ^ (x & (y ^ z)))
#define Maj(x,y,z)  ((x & y) | (z & (x | y)))

#define SHR(x, n)    (x >> n)
#define ROTR(x,n)    (SHR(x,n) | (x << (32 - n)))
#define S1(x)        (ROTR(x, 6) ^ ROTR(x,11) ^ ROTR(x,25))
#define S0(x)        (ROTR(x, 2) ^ ROTR(x,13) ^ ROTR(x,22))

#define s1(x)        (ROTR(x,17) ^ ROTR(x,19) ^  SHR(x,10))
#define s0(x)        (ROTR(x, 7) ^ ROTR(x,18) ^  SHR(x, 3))

#define SHA256_STEP(a,b,c,d,e,f,g,h,x,K)                 \
{                                                        \
    tmp1 = h + S1(e) + Ch(e,f,g) + K + x;                \
    tmp2 = S0(a) + Maj(a,b,c);                           \
    h  = tmp1 + tmp2;                                    \
    d += tmp1;                                           \
}

#define BE_LOAD32(n,b,i) (n) = byteswap(*(uint32_t *)(b + i))

static uint32_t byteswap(uint32_t x)
{
    x = (x & 0x0000FFFF) << 16 | (x & 0xFFFF0000) >> 16;
    x = (x & 0x00FF00FF) << 8 | (x & 0xFF00FF00) >> 8;  

    return x;
}

void sha256 (const uint8_t *in, uint32_t out[8])
{
    uint32_t tmp1, tmp2, a, b, c, d, e, f, g, h;
    uint32_t w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15;

    tmp1 = tmp2 = 0;
    w0 = w1 = w2 = w3 = w4 = w5 = w6 = w7 = w8 = w9 = w10 = w11 = w12 = w13 = w14 = w15 = 0;

    BE_LOAD32 (  w0, in,  0 );
    BE_LOAD32 (  w1, in,  4 );
    BE_LOAD32 (  w2, in,  8 );
    BE_LOAD32 (  w3, in, 12 );
    BE_LOAD32 (  w4, in, 16 );
    BE_LOAD32 (  w5, in, 20 );
    BE_LOAD32 (  w6, in, 24 );
    BE_LOAD32 (  w7, in, 28 );
    BE_LOAD32 (  w8, in, 32 );
    BE_LOAD32 (  w9, in, 36 );
    BE_LOAD32 ( w10, in, 40 );
    BE_LOAD32 ( w11, in, 44 );
    BE_LOAD32 ( w12, in, 48 );
    BE_LOAD32 ( w13, in, 52 );
    BE_LOAD32 ( w14, in, 56 );
    BE_LOAD32 ( w15, in, 60 );

    a = out[0];
    b = out[1];
    c = out[2];
    d = out[3];
    e = out[4];
    f = out[5];
    g = out[6];
    h = out[7];

    SHA256_STEP(a, b, c, d, e, f, g, h,  w0, 0x428a2f98);
    SHA256_STEP(h, a, b, c, d, e, f, g,  w1, 0x71374491);
    SHA256_STEP(g, h, a, b, c, d, e, f,  w2, 0xb5c0fbcf);
    SHA256_STEP(f, g, h, a, b, c, d, e,  w3, 0xe9b5dba5);
    SHA256_STEP(e, f, g, h, a, b, c, d,  w4, 0x3956c25b);
    SHA256_STEP(d, e, f, g, h, a, b, c,  w5, 0x59f111f1);
    SHA256_STEP(c, d, e, f, g, h, a, b,  w6, 0x923f82a4);
    SHA256_STEP(b, c, d, e, f, g, h, a,  w7, 0xab1c5ed5);
    SHA256_STEP(a, b, c, d, e, f, g, h,  w8, 0xd807aa98);
    SHA256_STEP(h, a, b, c, d, e, f, g,  w9, 0x12835b01);
    SHA256_STEP(g, h, a, b, c, d, e, f, w10, 0x243185be);
    SHA256_STEP(f, g, h, a, b, c, d, e, w11, 0x550c7dc3);
    SHA256_STEP(e, f, g, h, a, b, c, d, w12, 0x72be5d74);
    SHA256_STEP(d, e, f, g, h, a, b, c, w13, 0x80deb1fe);
    SHA256_STEP(c, d, e, f, g, h, a, b, w14, 0x9bdc06a7);
    SHA256_STEP(b, c, d, e, f, g, h, a, w15, 0xc19bf174);

    w0 = s1(w14) + w9 + s0(w1) + w0;
    SHA256_STEP(a, b, c, d, e, f, g, h,  w0, 0xe49b69c1);
    w1 = s1(w15) + w10 + s0(w2) + w1;
    SHA256_STEP(h, a, b, c, d, e, f, g,  w1, 0xefbe4786);
    w2 = s1(w0) + w11 + s0(w3) + w2;
    SHA256_STEP(g, h, a, b, c, d, e, f,  w2, 0x0fc19dc6);
    w3 = s1(w1) + w12 + s0(w4) + w3;
    SHA256_STEP(f, g, h, a, b, c, d, e,  w3, 0x240ca1cc);
    w4 = s1(w2) + w13 + s0(w5) + w4;
    SHA256_STEP(e, f, g, h, a, b, c, d,  w4, 0x2de92c6f);
    w5 = s1(w3) + w14 + s0(w6) + w5;
    SHA256_STEP(d, e, f, g, h, a, b, c,  w5, 0x4a7484aa);
    w6 = s1(w4) + w15 + s0(w7) + w6;
    SHA256_STEP(c, d, e, f, g, h, a, b,  w6, 0x5cb0a9dc);
    w7 = s1(w5) + w0 + s0(w8) + w7;
    SHA256_STEP(b, c, d, e, f, g, h, a,  w7, 0x76f988da);
    w8 = s1(w6) + w1 + s0(w9) + w8;
    SHA256_STEP(a, b, c, d, e, f, g, h,  w8, 0x983e5152);
    w9 = s1(w7) + w2 + s0(w10) + w9;
    SHA256_STEP(h, a, b, c, d, e, f, g,  w9, 0xa831c66d);
    w10 = s1(w8) + w3 + s0(w11) + w10;
    SHA256_STEP(g, h, a, b, c, d, e, f, w10, 0xb00327c8);
    w11 = s1(w9) + w4 + s0(w12) + w11;
    SHA256_STEP(f, g, h, a, b, c, d, e, w11, 0xbf597fc7);
    w12 = s1(w10) + w5 + s0(w13) + w12;
    SHA256_STEP(e, f, g, h, a, b, c, d, w12, 0xc6e00bf3);
    w13 = s1(w11) + w6 + s0(w14) + w13;
    SHA256_STEP(d, e, f, g, h, a, b, c, w13, 0xd5a79147);
    w14 = s1(w12) + w7 + s0(w15) + w14;
    SHA256_STEP(c, d, e, f, g, h, a, b, w14, 0x06ca6351);
    w15 = s1(w13) + w8 + s0(w0) + w15;
    SHA256_STEP(b, c, d, e, f, g, h, a, w15, 0x14292967);

    w0 = s1(w14) + w9 + s0(w1) + w0;
    SHA256_STEP(a, b, c, d, e, f, g, h,  w0, 0x27b70a85);
    w1 = s1(w15) + w10 + s0(w2) + w1;
    SHA256_STEP(h, a, b, c, d, e, f, g,  w1, 0x2e1b2138);
    w2 = s1(w0) + w11 + s0(w3) + w2;
    SHA256_STEP(g, h, a, b, c, d, e, f,  w2, 0x4d2c6dfc);
    w3 = s1(w1) + w12 + s0(w4) + w3;
    SHA256_STEP(f, g, h, a, b, c, d, e,  w3, 0x53380d13);
    w4 = s1(w2) + w13 + s0(w5) + w4;
    SHA256_STEP(e, f, g, h, a, b, c, d,  w4, 0x650a7354);
    w5 = s1(w3) + w14 + s0(w6) + w5;
    SHA256_STEP(d, e, f, g, h, a, b, c,  w5, 0x766a0abb);
    w6 = s1(w4) + w15 + s0(w7) + w6;
    SHA256_STEP(c, d, e, f, g, h, a, b,  w6, 0x81c2c92e);
    w7 = s1(w5) + w0 + s0(w8) + w7;
    SHA256_STEP(b, c, d, e, f, g, h, a,  w7, 0x92722c85);
    w8 = s1(w6) + w1 + s0(w9) + w8;
    SHA256_STEP(a, b, c, d, e, f, g, h,  w8, 0xa2bfe8a1);
    w9 = s1(w7) + w2 + s0(w10) + w9;
    SHA256_STEP(h, a, b, c, d, e, f, g,  w9, 0xa81a664b);
    w10 = s1(w8) + w3 + s0(w11) + w10;
    SHA256_STEP(g, h, a, b, c, d, e, f, w10, 0xc24b8b70);
    w11 = s1(w9) + w4 + s0(w12) + w11;
    SHA256_STEP(f, g, h, a, b, c, d, e, w11, 0xc76c51a3);
    w12 = s1(w10) + w5 + s0(w13) + w12;
    SHA256_STEP(e, f, g, h, a, b, c, d, w12, 0xd192e819);
    w13 = s1(w11) + w6 + s0(w14) + w13;
    SHA256_STEP(d, e, f, g, h, a, b, c, w13, 0xd6990624);
    w14 = s1(w12) + w7 + s0(w15) + w14;
    SHA256_STEP(c, d, e, f, g, h, a, b, w14, 0xf40e3585);
    w15 = s1(w13) + w8 + s0(w0) + w15;
    SHA256_STEP(b, c, d, e, f, g, h, a, w15, 0x106aa070);

    w0 = s1(w14) + w9 + s0(w1) + w0;
    SHA256_STEP(a, b, c, d, e, f, g, h,  w0, 0x19a4c116);
    w1 = s1(w15) + w10 + s0(w2) + w1;
    SHA256_STEP(h, a, b, c, d, e, f, g,  w1, 0x1e376c08);
    w2 = s1(w0) + w11 + s0(w3) + w2;
    SHA256_STEP(g, h, a, b, c, d, e, f,  w2, 0x2748774c);
    w3 = s1(w1) + w12 + s0(w4) + w3;
    SHA256_STEP(f, g, h, a, b, c, d, e,  w3, 0x34b0bcb5);
    w4 = s1(w2) + w13 + s0(w5) + w4;
    SHA256_STEP(e, f, g, h, a, b, c, d,  w4, 0x391c0cb3);
    w5 = s1(w3) + w14 + s0(w6) + w5;
    SHA256_STEP(d, e, f, g, h, a, b, c,  w5, 0x4ed8aa4a);
    w6 = s1(w4) + w15 + s0(w7) + w6;
    SHA256_STEP(c, d, e, f, g, h, a, b,  w6, 0x5b9cca4f);
    w7 = s1(w5) + w0 + s0(w8) + w7;
    SHA256_STEP(b, c, d, e, f, g, h, a,  w7, 0x682e6ff3);
    w8 = s1(w6) + w1 + s0(w9) + w8;
    SHA256_STEP(a, b, c, d, e, f, g, h,  w8, 0x748f82ee);
    w9 = s1(w7) + w2 + s0(w10) + w9;
    SHA256_STEP(h, a, b, c, d, e, f, g,  w9, 0x78a5636f);
    w10 = s1(w8) + w3 + s0(w11) + w10;
    SHA256_STEP(g, h, a, b, c, d, e, f, w10, 0x84c87814);
    w11 = s1(w9) + w4 + s0(w12) + w11;
    SHA256_STEP(f, g, h, a, b, c, d, e, w11, 0x8cc70208);
    w12 = s1(w10) + w5 + s0(w13) + w12;
    SHA256_STEP(e, f, g, h, a, b, c, d, w12, 0x90befffa);
    w13 = s1(w11) + w6 + s0(w14) + w13;
    SHA256_STEP(d, e, f, g, h, a, b, c, w13, 0xa4506ceb);
    w14 = s1(w12) + w7 + s0(w15) + w14;
    SHA256_STEP(c, d, e, f, g, h, a, b, w14, 0xbef9a3f7);
    w15 = s1(w13) + w8 + s0(w0) + w15;
    SHA256_STEP(b, c, d, e, f, g, h, a, w15, 0xc67178f2);

    out[0] += a;
    out[1] += b;
    out[2] += c;
    out[3] += d;
    out[4] += e;
    out[5] += f;
    out[6] += g;
    out[7] += h;
}

/* { dg-final { scan-assembler-not {vset} } } */
