/* { dg-do run { target { lp64 && avx2_runtime } } } */
/* { dg-options "-O3 -mavx2" } */

#include <stdint.h>
#include <string.h>
#include <immintrin.h>

#define SPX_N 16

typedef struct {
    uint8_t pub_seed[SPX_N];
} spx_ctx;

typedef float __m256_u __attribute__ ((__vector_size__ (32), __may_alias__, __aligned__ (1)));
typedef double __m256d_u __attribute__ ((__vector_size__ (32), __may_alias__, __aligned__ (1)));
typedef long long __m256i_u __attribute__ ((__vector_size__ (32), __may_alias__, __aligned__ (1)));

__attribute__((noinline))
void Keccak(__m256i *states) { }

__attribute__((noipa))
void capture(uint8_t *v) { }

static void thashx4(uint64_t *out0,
             uint64_t *out1,
             uint64_t *out2,
             uint64_t *out3,
             unsigned int inblocks,
             const uint64_t* pub_seed) {
    if (inblocks == 1 || inblocks == 2) {
        __m256i state[25];
        for (int i = 0; i < 25; i++) {
            state[i] = _mm256_set1_epi64x(0);
        }
        for (int i = 0; i < 2; i++) {
            state[i] = _mm256_set1_epi64x(pub_seed[i]);
        }

        /* Domain separator and padding. */
        for (size_t i = 4 * inblocks + 4; i < 16; i++) {
            state[i] = _mm256_set1_epi64x(0);
        }

        Keccak(&state[0]);

        for (int i = 0; i < SPX_N / 8; i++) {
            out0[i] = _mm256_extract_epi64(state[i], 0);
            out1[i] = _mm256_extract_epi64(state[i], 1);
            out2[i] = _mm256_extract_epi64(state[i], 2);
            out3[i] = _mm256_extract_epi64(state[i], 3);
        }
    } else {
        unsigned char buf0[inblocks * SPX_N];
        unsigned char buf1[inblocks * SPX_N];
        unsigned char buf2[inblocks * SPX_N];

        memcpy(buf0, pub_seed, SPX_N);
        memcpy(buf1, pub_seed, SPX_N);
        memcpy(buf2, pub_seed, SPX_N);

        capture(buf0);
        capture(buf1);
        capture(buf2);
    }
}

static
void wots_gen_leafx4(const spx_ctx *ctx) {
	uint64_t dest[4];

	thashx4(dest, dest, dest, dest, 1, (const uint64_t *) ctx->pub_seed);
	thashx4(dest, dest, dest, dest, 3, (const uint64_t *) ctx->pub_seed);
}

void treehashx4_v2(const spx_ctx *ctx,
                void (*gen_leafx4)( const spx_ctx *),
                uint32_t* tree_addrx4
                ) {
    for (int i = 0; i < 2; i++) {
        gen_leafx4( ctx );
    }
}

__attribute__((noipa))
void crypto_sign_signature(uint64_t *sg, uint8_t enable, const uint8_t *sk)
{
  spx_ctx ctx;

  memcpy(ctx.pub_seed, sk, SPX_N);

  const uint64_t* ptr = (const uint64_t *)&ctx.pub_seed[0];
  thashx4(sg, sg, sg, sg, 1, ptr);

  if (!enable)
    return;

  uint32_t tree_addrx4[32] = { 0 };

  treehashx4_v2(&ctx, wots_gen_leafx4, tree_addrx4);
}

#define length_secret_key 64
#define length_signature 64

int main() {
  uint8_t secret_key[length_secret_key];
  uint64_t signature[length_signature];
  memset(secret_key, 0, length_secret_key);
  memset(signature, 0, length_signature * sizeof(uint64_t));

  crypto_sign_signature(signature, 0, secret_key);
  
  for (int i = 0; i < length_signature; ++i)
    if (signature[i] != 0)
      __builtin_abort ();
  return 0;
}
