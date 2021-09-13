// { dg-do compile }
// { dg-additional-options "-fvect-cost-model=cheap" }
// { dg-additional-options "-mavx2" { target x86_64-*-* i?86-*-* } }

#include <stdint.h>
#include <stdlib.h>

inline uint32_t make_uint32(uint8_t i0, uint8_t i1, uint8_t i2, uint8_t i3)
{
  return ((static_cast<uint32_t>(i0) << 24) |
	  (static_cast<uint32_t>(i1) << 16) |
	  (static_cast<uint32_t>(i2) <<  8) |
	  (static_cast<uint32_t>(i3)));
}

inline uint32_t load_be(const uint8_t in[], size_t off)
{
  in += off * sizeof(uint32_t);
  return make_uint32(in[0], in[1], in[2], in[3]);
}

template<typename T>
inline void load_be(const uint8_t in[],
		    T& x0, T& x1, T& x2, T& x3,
		    T& x4, T& x5, T& x6, T& x7)
{
  x0 = load_be(in, 0);
  x1 = load_be(in, 1);
  x2 = load_be(in, 2);
  x3 = load_be(in, 3);
  x4 = load_be(in, 4);
  x5 = load_be(in, 5);
  x6 = load_be(in, 6);
  x7 = load_be(in, 7);
}

inline void store_be(uint32_t in, uint8_t out[4])
{
  uint32_t o = __builtin_bswap32 (in);
  __builtin_memcpy (out, &o, sizeof (uint32_t));
}

template<typename T>
inline void store_be(uint8_t out[], T x0, T x1, T x2, T x3,
		     T x4, T x5, T x6, T x7)
{
  store_be(x0, out + (0 * sizeof(T)));
  store_be(x1, out + (1 * sizeof(T)));
  store_be(x2, out + (2 * sizeof(T)));
  store_be(x3, out + (3 * sizeof(T)));
  store_be(x4, out + (4 * sizeof(T)));
  store_be(x5, out + (5 * sizeof(T)));
  store_be(x6, out + (6 * sizeof(T)));
  store_be(x7, out + (7 * sizeof(T)));
}

#define BLOCK_SIZE 8
void encrypt_n(const uint8_t in[], uint8_t out[], size_t blocks, uint32_t *EK)
{
  const size_t blocks4 = blocks / 4;

  for (size_t i = 0; i < blocks4; i++)
    {
      uint32_t L0, R0, L1, R1, L2, R2, L3, R3;
      load_be(in + 4*BLOCK_SIZE*i, L0, R0, L1, R1, L2, R2, L3, R3);

      for(size_t r = 0; r != 32; ++r)
	{
	  L0 += (((R0 << 4) ^ (R0 >> 5)) + R0) ^ EK[2*r];
	  L1 += (((R1 << 4) ^ (R1 >> 5)) + R1) ^ EK[2*r];
	  L2 += (((R2 << 4) ^ (R2 >> 5)) + R2) ^ EK[2*r];
	  L3 += (((R3 << 4) ^ (R3 >> 5)) + R3) ^ EK[2*r];

	  R0 += (((L0 << 4) ^ (L0 >> 5)) + L0) ^ EK[2*r+1];
	  R1 += (((L1 << 4) ^ (L1 >> 5)) + L1) ^ EK[2*r+1];
	  R2 += (((L2 << 4) ^ (L2 >> 5)) + L2) ^ EK[2*r+1];
	  R3 += (((L3 << 4) ^ (L3 >> 5)) + L3) ^ EK[2*r+1];
	}

      store_be(out + 4*BLOCK_SIZE*i, L0, R0, L1, R1, L2, R2, L3, R3);
    }
}

// This used to work on { target x86_64-*-* i?86-*-* } but a fix in SLP
// discovery makes us trip over the threshold again.
// { dg-final { scan-tree-dump-times "not vectorized: vectorization is not profitable" 2 "slp1" { xfail *-*-* } } }
