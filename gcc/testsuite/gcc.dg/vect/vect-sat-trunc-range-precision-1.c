/* { dg-require-effective-target bitint } */
/* { dg-additional-options "-O3 -fvect-cost-model=unlimited" } */

typedef __INT8_TYPE__ int8_t;
typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef signed _BitInt(16) int16b_t;
typedef unsigned _BitInt(17) uint17_t;

#define N 259
#define SAT_VALUE(OUT, X, MAX) ((OUT) (-((OUT) ((X) < 0)) ^ (OUT) MAX))

__attribute__((noipa))
static void
wide_range (int8_t *__restrict out, const int16b_t *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int16b_t source = in[i];
      uint32_t range = (uint32_t) source + 128U;
      out[i] = (range > 255U
		? SAT_VALUE (int8_t, source, 127) : (int8_t) source);
    }
}

__attribute__((noipa))
static void
narrow_range (int16_t *__restrict out, const int32_t *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int32_t source = in[i];
      uint17_t range = (uint17_t) source + (uint17_t) 32768;
      out[i] = (range > (uint17_t) 65535
		? SAT_VALUE (int16_t, source, 32767) : (int16_t) source);
    }
}

__attribute__((noipa, optimize ("O0")))
static void
wide_range_ref (int8_t *out, const int16b_t *in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int16b_t source = in[i];
      if (source < -128)
	out[i] = -128;
      else if (source > 127)
	out[i] = 127;
      else
	out[i] = (int8_t) source;
    }
}

__attribute__((noipa, optimize ("O0")))
static void
narrow_range_ref (int16_t *out, const int32_t *in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int32_t source = in[i];
      uint17_t range = (uint17_t) source + (uint17_t) 32768;
      out[i] = (range > (uint17_t) 65535
		? SAT_VALUE (int16_t, source, 32767) : (int16_t) source);
    }
}

static void
check_wide_range (void)
{
  int8_t out[N];
  int8_t ref[N];
  int16b_t in[N];

  for (int i = 0; i < N; ++i)
    in[i] = (uint16_t) i * 40503U + 97U;

  for (int n = 0; n <= N; ++n)
    {
      for (int i = 0; i < N; ++i)
	out[i] = ref[i] = 23;
      wide_range (out, in, n);
      wide_range_ref (ref, in, n);
      for (int i = 0; i < N; ++i)
	if (out[i] != ref[i])
	  __builtin_abort ();
    }
}

static void
check_narrow_range (void)
{
  int16_t out[N];
  int16_t ref[N];
  int32_t in[N];

  for (int i = 0; i < N; ++i)
    in[i] = (int32_t) ((uint32_t) i * 2654435761U + 1013904223U);
  in[0] = 131072;

  for (int n = 0; n <= N; ++n)
    {
      for (int i = 0; i < N; ++i)
	out[i] = ref[i] = 23;
      narrow_range (out, in, n);
      narrow_range_ref (ref, in, n);
      for (int i = 0; i < N; ++i)
	if (out[i] != ref[i])
	  __builtin_abort ();
    }
}

int
main (void)
{
  check_wide_range ();
  check_narrow_range ();
  return 0;
}
