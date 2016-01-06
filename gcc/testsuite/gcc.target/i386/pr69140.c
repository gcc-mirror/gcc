/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mincoming-stack-boundary=3" } */

typedef struct {
  unsigned int buf[4];
  unsigned char in[64];
} MD4_CTX;

static void
MD4Transform (unsigned int buf[4], const unsigned int in[16])
{
  unsigned int a, b, c, d;
  (b) += ((((c)) & ((d))) | ((~(c)) & ((a)))) + (in[7]);
  (a) += ((((b)) & ((c))) | ((~(b)) & ((d)))) + (in[8]);
  (d) += ((((a)) & ((b))) | ((~(a)) & ((c)))) + (in[9]);
  buf[3] += d;
}

void __attribute__((ms_abi))
MD4Update (MD4_CTX *ctx, const unsigned char *buf)
{
  MD4Transform( ctx->buf, (unsigned int *)ctx->in);
  MD4Transform( ctx->buf, (unsigned int *)ctx->in);
}
