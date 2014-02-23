/* { dg-options "-O2 -fPIC" } */

typedef struct test_struct
{
    unsigned long long h[8];
    unsigned long long Nl,Nh;
    union {
        unsigned long long d[16];
        unsigned char p[(16*8)];
    } u;
    unsigned int num,md_len;
} TEST_STRUCT;

static const unsigned long long K512[12] = {
    0x428a2f98d728ae22,0x7137449123ef65cd,
    0xb5c0fbcfec4d3b2f,0xe9b5dba58189dbbc,
    0x3956c25bf348b538,0x59f111f1b605d019,
    0x923f82a4af194f9b,0xab1c5ed5da6d8118,
    0xd807aa98a3030242,0x12835b0145706fbe,
    0x243185be4ee4b28c,0x550c7dc3d5ffb4e2};

#define ROTR(x,s)   (((x)>>s) | (x)<<(64-s))
#define Sigma0(x)   (ROTR((x),28) ^ ROTR((x),34) ^ ROTR((x),39))
#define Sigma1(x)   (ROTR((x),14) ^ ROTR((x),18) ^ ROTR((x),41))
#define Ch(x,y,z)   (((x) & (y)) ^ ((~(x)) & (z)))
#define Maj(x,y,z)  (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))

#define ROUND_00_15(i,a,b,c,d,e,f,g,h)  do {    \
    T1 += h + Sigma1(e) + Ch(e,f,g) + K512[i];  \
    h = Sigma0(a) + Maj(a,b,c);         \
    d += T1;    h += T1;        } while (0)

#define ROUND_16_80(i,a,b,c,d,e,f,g,h,X)    do {    \
    T1 = X[(i)&0x0f] += s0 + s1 + X[(i+9)&0x0f];    \
    ROUND_00_15(i,a,b,c,d,e,f,g,h);     } while (0)

static void testfunc1 (TEST_STRUCT *ctx, const void *in, unsigned int num)
{
    const unsigned long long *W=in;
    unsigned long long  a,b,c,d,e,f,g,h,s0,s1,T1;
    unsigned long long  X[16];
    int i;

    while (num--) {

        T1 = X[0] = W[0];   ROUND_00_15(0,a,b,c,d,e,f,g,h);
        T1 = X[1] = W[1];   ROUND_00_15(1,h,a,b,c,d,e,f,g);
        T1 = X[2] = W[2];   ROUND_00_15(2,g,h,a,b,c,d,e,f);
        T1 = X[3] = W[3];   ROUND_00_15(3,f,g,h,a,b,c,d,e);
        T1 = X[4] = W[4];   ROUND_00_15(4,e,f,g,h,a,b,c,d);
        T1 = X[5] = W[5];   ROUND_00_15(5,d,e,f,g,h,a,b,c);
        T1 = X[6] = W[6];   ROUND_00_15(6,c,d,e,f,g,h,a,b);
        T1 = X[7] = W[7];   ROUND_00_15(7,b,c,d,e,f,g,h,a);
        T1 = X[8] = W[8];   ROUND_00_15(8,a,b,c,d,e,f,g,h);
        T1 = X[9] = W[9];   ROUND_00_15(9,h,a,b,c,d,e,f,g);

        for (i=16;i<80;i+=8)
        {
            ROUND_16_80(i+0,a,b,c,d,e,f,g,h,X);
        }

        ctx->h[4] += e; ctx->h[5] += f; ctx->h[6] += g; ctx->h[7] += h;
    }
}

int testfunc2 (TEST_STRUCT *c, const void *_data, unsigned int len)
{
    const unsigned char *data=(const unsigned char *)_data;

    unsigned char *p=(unsigned char *)c->u.p;

    testfunc1 (c,p,0);
    testfunc1 (c,data,len/sizeof(c->u));
}
