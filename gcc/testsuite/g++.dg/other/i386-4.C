/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O1 -msse2" } */
/* { dg-require-effective-target sse2 } */

#include <xmmintrin.h>

void ConvertFloatSSE (void *inBuff, void *outBuff, int len)
{
  unsigned char *inByteBuffer = reinterpret_cast < unsigned char *>(inBuff);
  float *outFloatBuffer = reinterpret_cast < float *>(outBuff);

  for (int i = 0; i < len / 4; i++)
    {
      __m128i register1 =
	_mm_set_epi8 (*inByteBuffer, *(inByteBuffer + 1),
		      *(inByteBuffer + 2), 0,
		      *(inByteBuffer + 3), *(inByteBuffer + 4),
		      *(inByteBuffer + 5), 0,
		      *(inByteBuffer + 6), *(inByteBuffer + 7),
		      *(inByteBuffer + 8), 0,
		      *(inByteBuffer + 9), *(inByteBuffer + 10),
		      *(inByteBuffer + 11), 0);
      __m128i register2 = _mm_srai_epi32 (register1, 8);
      __m128 register3 = _mm_cvtepi32_ps (register2);

      _mm_store_ps (outFloatBuffer, register3);
      outFloatBuffer += 4;
      inByteBuffer += 12;
    }
}
