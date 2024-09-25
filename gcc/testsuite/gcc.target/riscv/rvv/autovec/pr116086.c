/* { dg-do run } */
/* { dg-require-effective-target riscv_v } */
/* { dg-require-effective-target rvv_zvl256b_ok } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-max-lmul=m2" } */

typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;

typedef struct
{
    uint64_t length;
    uint64_t state[8];
    uint32_t curlen;
    unsigned char buf[128];
} sha512_state;

static uint64_t load64(const unsigned char* y)
{
    uint64_t res = 0;
    for(int i = 0; i != 8; ++i)
        res |= (uint64_t)(y[i]) << ((7-i) * 8);
    return res;
}

static const uint64_t K[80] =
{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

__attribute__ ((noipa))
static void sha_compress(sha512_state *md, const unsigned char *buf)
{
    uint64_t S[8], W[80];

    for(int i = 0; i < 8; i++)
      S[i] = 0;

    // Copy the state into 1024-bits into W[0..15]
    for(int i = 0; i < 16; i++)
      W[i] = load64(buf + (8*i));

    // Fill W[16..79]
    for(int i = 16; i < 80; i++)
      W[i] = W[i - 2] + W[i - 7] + W[i - 15] + W[i - 16];

    S[7] = W[72];

     // Feedback
    for(int i = 0; i < 8; i++)
      md->state[i] = md->state[i] + S[i];
}

int main ()
{
  sha512_state md;
  md.curlen = 0;
  md.length = 0;
  md.state[0] = 0;
  md.state[1] = 0;
  md.state[2] = 0;
  md.state[3] = 0;
  md.state[4] = 0;
  md.state[5] = 0;
  md.state[6] = 0;
  md.state[7] = 0;

  for (int i = 0; i < 128; i++)
    md.buf[i] = 0;

  md.buf[md.curlen++] = (unsigned char)0x80;

  sha_compress (&md, md.buf);

  if (md.state[7] != 0x8000000000000000ULL)
    __builtin_abort ();

  return 0;
}
