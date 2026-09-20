/* { dg-require-effective-target bitint } */
/* { dg-additional-options "-O3 -fvect-cost-model=unlimited" } */

typedef __INT8_TYPE__ int8_t;
typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;
typedef signed _BitInt(16) int16b_t;
typedef unsigned _BitInt(12) uint12b_t;
typedef unsigned _BitInt(16) uint16b_t;
typedef unsigned _BitInt(17) uint17b_t;
typedef unsigned _BitInt(24) uint24b_t;

#define N 259

#define SHIFT_SAT_VALUE(OUT, X, SHIFT, MAX) \
  ((OUT) ((OUT) ((X) >> (SHIFT)) ^ (OUT) (MAX)))
#define COMPARE_SAT_VALUE(OUT, X, SHIFT, MAX) \
  ((OUT) (-((OUT) ((X) < 0)) ^ (OUT) (MAX)))

#define DEF_CLIP(NAME, SAT, OUT, IN, UIN, SHIFT, SIGN_SHIFT, OFFSET, LIMIT, \
		 MAX)                                                        \
  __attribute__((noipa))                                                   \
  static void                                                              \
  NAME (OUT *__restrict out, const IN *__restrict in, int n)               \
  {                                                                        \
    for (int i = 0; i < n; ++i)                                           \
      {                                                                    \
	IN source = in[i];                                                  \
	IN shifted = source >> SHIFT;                                       \
	UIN range = (UIN) shifted + (UIN) OFFSET;                            \
	out[i] = (range > (UIN) LIMIT                                       \
		  ? SAT (OUT, source, SIGN_SHIFT, MAX) : (OUT) shifted);      \
      }                                                                    \
  }

#define DEF_REF(NAME, OUT, IN, SHIFT, MIN, MAX)                           \
  __attribute__((noipa, optimize ("O0")))                                \
  static void                                                             \
  NAME (OUT *out, const IN *in, int n)                                    \
  {                                                                       \
    for (int i = 0; i < n; ++i)                                          \
      {                                                                   \
	IN shifted = in[i] >> SHIFT;                                       \
	if (shifted < (IN) MIN)                                            \
	  out[i] = (OUT) MIN;                                              \
	else if (shifted > (IN) MAX)                                      \
	  out[i] = (OUT) MAX;                                              \
	else                                                               \
	  out[i] = (OUT) shifted;                                          \
      }                                                                   \
  }

DEF_CLIP (clip_s16_s8, SHIFT_SAT_VALUE, int8_t, int16b_t, uint16b_t,
	  3, 15, 128, 255, 127)
DEF_CLIP (clip_s32_s16, COMPARE_SAT_VALUE, int16_t, int32_t, uint32_t,
	  8, 31, 32768U, 65535U, 32767)
DEF_CLIP (clip_s64_s32, SHIFT_SAT_VALUE, int32_t, int64_t, uint64_t,
	  13, 63, 2147483648ULL, 4294967295ULL, 2147483647)
DEF_CLIP (clip_s16_s8_wide_unshifted, SHIFT_SAT_VALUE, int8_t, int16b_t,
	  uint32_t, 0, 15, 128, 255, 127)
DEF_CLIP (clip_s16_s8_wide_shifted, SHIFT_SAT_VALUE, int8_t, int16b_t,
	  uint32_t, 3, 15, 128, 255, 127)
DEF_CLIP (clip_s32_s16_reduced_range, SHIFT_SAT_VALUE, int16_t, int32_t,
	  uint24b_t, 8, 31, 32768, 65535, 32767)

DEF_REF (ref_s16_s8, int8_t, int16b_t, 3, -128, 127)
DEF_REF (ref_s32_s16, int16_t, int32_t, 8, -32768, 32767)
DEF_REF (ref_s64_s32, int32_t, int64_t, 13,
	 -2147483647 - 1, 2147483647)
DEF_REF (ref_s16_s8_wide_unshifted, int8_t, int16b_t, 0, -128, 127)
DEF_REF (ref_s16_s8_wide_shifted, int8_t, int16b_t, 3, -128, 127)
DEF_REF (ref_s32_s16_reduced_range, int16_t, int32_t, 8, -32768, 32767)

/* This range calculation can wrap a shifted value that still fits in the
   shifted input type.  */

__attribute__((noipa))
static void
narrow_range (int16_t *__restrict out, const int32_t *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int32_t source = in[i];
      int32_t shifted = source >> 1;
      uint17b_t range = (uint17b_t) shifted + (uint17b_t) 32768;
      out[i] = (range > (uint17b_t) 65535
		? (int16_t) (source >> 31) ^ 32767 : (int16_t) shifted);
    }
}

__attribute__((noipa, optimize ("O0")))
static void
ref_narrow_range (int16_t *out, const int32_t *in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int32_t source = in[i];
      int32_t shifted = source >> 1;
      uint17b_t range = (uint17b_t) shifted + (uint17b_t) 32768;
      out[i] = (range > (uint17b_t) 65535
		? (int16_t) (source >> 31) ^ 32767 : (int16_t) shifted);
    }
}

/* These wrapped constants do not describe a saturating truncation.  */

__attribute__((noipa))
static void
wrapped_output_range (int16_t *__restrict out, const int32_t *__restrict in,
		      int n)
{
  for (int i = 0; i < n; ++i)
    {
      int32_t source = in[i];
      int32_t shifted = source >> 20;
      uint12b_t range = (uint12b_t) shifted + (uint12b_t) 32767;
      out[i] = (range > (uint12b_t) 65534
		? (int16_t) (source >> 31) ^ 32767 : (int16_t) shifted);
    }
}

__attribute__((noipa, optimize ("O0")))
static void
ref_wrapped_output_range (int16_t *out, const int32_t *in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int32_t source = in[i];
      int32_t shifted = source >> 20;
      uint12b_t range = (uint12b_t) shifted + (uint12b_t) 32767;
      out[i] = (range > (uint12b_t) 65534
		? (int16_t) (source >> 31) ^ 32767 : (int16_t) shifted);
    }
}

typedef void (*clip_fn) (int16_t *, const int32_t *, int);

static void
check_invalid (clip_fn fn, clip_fn ref_fn, int32_t special)
{
  int16_t out[N];
  int16_t ref[N];
  int32_t in[N];

  for (int i = 0; i < N; ++i)
    in[i] = (int32_t) ((uint32_t) i * 2654435761U + 1013904223U);
  in[0] = special;

  for (int n = 0; n <= N; ++n)
    {
      for (int i = 0; i < N; ++i)
	out[i] = ref[i] = 23;
      fn (out, in, n);
      ref_fn (ref, in, n);
      for (int i = 0; i < N; ++i)
	if (out[i] != ref[i])
	  __builtin_abort ();
    }
}

#define CHECK(NAME, REF, OUT, IN, INIT, SHIFT, MIN, MAX)                  \
  do                                                                     \
    {                                                                    \
      OUT out[N];                                                        \
      OUT ref[N];                                                        \
      IN in[N];                                                          \
      IN scale = (IN) 1 << (SHIFT);                                      \
      for (int i = 0; i < N; ++i)                                      \
	in[i] = (IN) (INIT);                                               \
      in[0] = ((IN) (MIN) - 1) * scale;                                 \
      in[1] = (IN) (MIN) * scale;                                       \
      in[2] = (IN) -1 * scale;                                          \
      in[3] = 0;                                                        \
      in[4] = (IN) (MAX) * scale;                                       \
      in[5] = ((IN) (MAX) + 1) * scale;                                 \
      for (int n = 0; n <= N; ++n)                                     \
	{                                                                 \
	  for (int i = 0; i < N; ++i)                                    \
	    out[i] = ref[i] = (OUT) 23;                                   \
	  NAME (out, in, n);                                              \
	  REF (ref, in, n);                                               \
	  for (int i = 0; i < N; ++i)                                    \
	    if (out[i] != ref[i])                                         \
	      __builtin_abort ();                                         \
	}                                                                 \
    }                                                                    \
  while (0)

int
main (void)
{
  CHECK (clip_s16_s8, ref_s16_s8, int8_t, int16b_t,
	 (uint16b_t) i * (uint16b_t) 40503 + (uint16b_t) 97,
	 3, -128, 127);
  CHECK (clip_s32_s16, ref_s32_s16, int16_t, int32_t,
	 (uint32_t) i * 2654435761U + 1013904223U,
	 8, -32768, 32767);
  CHECK (clip_s64_s32, ref_s64_s32, int32_t, int64_t,
	 (uint64_t) i * 11400714819323198485ULL
	 + 13787848793156543929ULL,
	 13, -2147483647 - 1, 2147483647);
  CHECK (clip_s16_s8_wide_unshifted, ref_s16_s8_wide_unshifted,
	 int8_t, int16b_t,
	 (uint16b_t) i * (uint16b_t) 40503 + (uint16b_t) 97,
	 0, -128, 127);
  CHECK (clip_s16_s8_wide_shifted, ref_s16_s8_wide_shifted,
	 int8_t, int16b_t,
	 (uint16b_t) i * (uint16b_t) 40503 + (uint16b_t) 97,
	 3, -128, 127);
  CHECK (clip_s32_s16_reduced_range, ref_s32_s16_reduced_range,
	 int16_t, int32_t,
	 (uint32_t) i * 2654435761U + 1013904223U,
	 8, -32768, 32767);
  check_invalid (narrow_range, ref_narrow_range, 196608);
  check_invalid (wrapped_output_range, ref_wrapped_output_range, 0);
  return 0;
}
