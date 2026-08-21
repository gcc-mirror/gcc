/* { dg-do compile { target bitint } } */
/* { dg-options "-O3 -march=armv8-a" } */
/* { dg-additional-options "-mmax-vectorization --param=vect-epilogues-nomask=0 -fdump-tree-vect-details" } */

typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef unsigned _BitInt(12) uint12b_t;
typedef unsigned _BitInt(17) uint17b_t;

__attribute__((noipa))
void
different_source (int16_t *__restrict out, const int32_t *__restrict in,
		  const int32_t *__restrict signs, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int32_t source = in[i];
      int32_t shifted = source >> 8;
      uint32_t range = (uint32_t) shifted + 32768U;
      int32_t sign_source = signs[i];
      out[i] = (range > 65535U
		? (int16_t) (sign_source >> 31) ^ 32767
		: (int16_t) shifted);
    }
}

__attribute__((noipa))
void
logical_shift (int16_t *__restrict out, const uint32_t *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      uint32_t source = in[i];
      uint32_t shifted = source >> 8;
      uint32_t range = shifted + 32768U;
      out[i] = (range > 65535U
		? (int16_t) ((int32_t) source >> 31) ^ 32767
		: (int16_t) shifted);
    }
}

__attribute__((noipa))
void
variable_count (int16_t *__restrict out, const int32_t *__restrict in,
		const uint16_t *__restrict counts, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int32_t source = in[i];
      int32_t shifted = source >> (counts[i] & 31);
      uint32_t range = (uint32_t) shifted + 32768U;
      out[i] = (range > 65535U
		? (int16_t) (source >> 31) ^ 32767
		: (int16_t) shifted);
    }
}

__attribute__((noipa))
void
wrong_sign_count (int16_t *__restrict out, const int32_t *__restrict in,
		  int n)
{
  for (int i = 0; i < n; ++i)
    {
      int32_t source = in[i];
      int32_t shifted = source >> 8;
      uint32_t range = (uint32_t) shifted + 32768U;
      out[i] = (range > 65535U
		? (int16_t) (source >> 30) ^ 32767
		: (int16_t) shifted);
    }
}

__attribute__((noipa))
void
wrong_range (int16_t *__restrict out, const int32_t *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int32_t source = in[i];
      int32_t shifted = source >> 8;
      uint32_t range = (uint32_t) shifted + 32767U;
      out[i] = (range > 65535U
		? (int16_t) (source >> 31) ^ 32767
		: (int16_t) shifted);
    }
}

__attribute__((noipa))
void
narrow_range (int16_t *__restrict out, const int32_t *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int32_t source = in[i];
      int32_t shifted = source >> 1;
      uint17b_t range = (uint17b_t) shifted + (uint17b_t) 32768;
      out[i] = (range > (uint17b_t) 65535
		? (int16_t) (source >> 31) ^ 32767
		: (int16_t) shifted);
    }
}

__attribute__((noipa))
void
wrapped_output_range (int16_t *__restrict out,
		      const int32_t *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int32_t source = in[i];
      int32_t shifted = source >> 20;
      uint12b_t range = (uint12b_t) shifted + (uint12b_t) 32767;
      out[i] = (range > (uint12b_t) 65534
		? (int16_t) (source >> 31) ^ 32767
		: (int16_t) shifted);
    }
}

/* { dg-final { scan-tree-dump-not "sat_trunc pattern recognized" "vect" } } */
