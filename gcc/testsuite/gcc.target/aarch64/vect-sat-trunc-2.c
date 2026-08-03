/* { dg-do run } */
/* { dg-options "-O3" } */

typedef __UINT16_TYPE__ u16; typedef __UINT8_TYPE__ u8;
typedef __UINT32_TYPE__ u32; typedef __UINT64_TYPE__ u64;
typedef __INT16_TYPE__ i16; typedef __INT8_TYPE__ i8;
typedef __INT32_TYPE__ i32; typedef __INT64_TYPE__ i64;
#define N 137

#define DEFU(name, WT, NT, NMAX)					\
  static inline NT clip_##name (WT x)					\
  { return x & (WT) ~(WT) NMAX ? (NT) NMAX : (NT) x; }			\
  __attribute__((noipa)) void name (NT *__restrict r, WT *__restrict x, int n) \
  { for (int i = 0; i < n; i++) r[i] = clip_##name (x[i]); }		\
  __attribute__((noipa, optimize ("O0")))				\
  void name##_ref (NT *__restrict r, WT *__restrict x, int n)		\
  { for (int i = 0; i < n; i++) r[i] = clip_##name (x[i]); }

#define DEFS(name, WT, NT, NMIN, NMAX)					\
  static inline NT clip_##name (WT x)					\
  { NT t = (NT) x;							\
    return (WT) NMIN <= x && x <= (WT) NMAX ? t : x < 0 ? NMIN : NMAX; } \
  __attribute__((noipa)) void name (NT *__restrict r, WT *__restrict x, int n) \
  { for (int i = 0; i < n; i++) r[i] = clip_##name (x[i]); }		\
  __attribute__((noipa, optimize ("O0")))				\
  void name##_ref (NT *__restrict r, WT *__restrict x, int n)		\
  { for (int i = 0; i < n; i++) r[i] = clip_##name (x[i]); }

DEFU (u16to8, u16, u8, 255)
DEFU (u32to16, u32, u16, 65535)
DEFU (u64to32, u64, u32, 0xffffffffu)
DEFS (i16to8, i16, i8, -128, 127)
DEFS (i32to16, i32, i16, -32768, 32767)
DEFS (i64to32, i64, i32, (i32) 0x80000000, 0x7fffffff)

static u16 a16[N]; static u32 a32[N]; static u64 a64[N];
static u8 d8[N], e8[N]; static u16 d16[N], e16[N]; static u32 d32[N], e32[N];
static unsigned long seed = 7;
static unsigned rnd (void) { seed = seed * 6364136223846793005UL + 1; return (unsigned)(seed >> 33); }
#define CHK(d, e, n) for (int i = 0; i < n; i++) if (d[i] != e[i]) __builtin_abort ();
int main (void)
{
  for (int r = 0; r < 200; r++)
    {
      for (int i = 0; i < N; i++)
	{ unsigned v = rnd ();
	  a16[i] = (u16) v; a32[i] = v; a64[i] = ((u64) v << 32) | rnd ();
	  if ((i & 7) == 0) { a16[i] = 0xffff; a32[i] = 0xffffffffu; a64[i] = ~0UL; } }
      u16to8 (d8, a16, N); u16to8_ref (e8, a16, N); CHK (d8, e8, N)
      u32to16 (d16, a32, N); u32to16_ref (e16, a32, N); CHK (d16, e16, N)
      u64to32 (d32, a64, N); u64to32_ref (e32, a64, N); CHK (d32, e32, N)
      i16to8 ((i8 *) d8, (i16 *) a16, N); i16to8_ref ((i8 *) e8, (i16 *) a16, N); CHK (d8, e8, N)
      i32to16 ((i16 *) d16, (i32 *) a32, N); i32to16_ref ((i16 *) e16, (i32 *) a32, N); CHK (d16, e16, N)
      i64to32 ((i32 *) d32, (i64 *) a64, N); i64to32_ref ((i32 *) e32, (i64 *) a64, N); CHK (d32, e32, N)
    }
  return 0;
}
