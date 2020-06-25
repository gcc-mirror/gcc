/* { dg-do compile } */

typedef unsigned short uint16_t;
typedef short __v8hi __attribute__ ((__vector_size__ (16)));
typedef long long __m128i __attribute__ ((__vector_size__ (16), __may_alias__));
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__))
_mm_set_epi16 (short __q7, short __q6, short __q5, short __q4,
	       short __q3, short __q2, short __q1, short __q0)
{
  return __extension__ (__m128i)(__v8hi){
      __q0, __q1, __q2, __q3, __q4, __q5, __q6, __q7 };
}
void gcm_HashMult_hw(__m128i *x, const unsigned char *buf, unsigned int count)
{
  unsigned i;
  __m128i bin __attribute__((aligned(16)));
  for (i = 0; i < count; i++, buf += 16)
    {
      bin = _mm_set_epi16(((uint16_t)buf[0] << 8) | buf[1],
			  ((uint16_t)buf[2] << 8) | buf[3],
			  ((uint16_t)buf[4] << 8) | buf[5],
			  ((uint16_t)buf[6] << 8) | buf[7],
			  ((uint16_t)buf[8] << 8) | buf[9],
			  ((uint16_t)buf[10] << 8) | buf[11],
			  ((uint16_t)buf[12] << 8) | buf[13],
			  ((uint16_t)buf[14] << 8) | buf[15]);
      *(x++) = bin;
    }
}
