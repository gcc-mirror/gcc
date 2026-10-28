/* Common body for the PR target/122382 run tests.  Check that vectorized
   float <-> _Float16 conversions agree with scalar reference loops,
   including lane order, and that nothing is written past the end.  */

#include <stdlib.h>
#include <string.h>

#define MAXN 128

#define REF_ATTR __attribute__ ((noipa, optimize ("no-tree-vectorize")))

#define DEF(N)								\
__attribute__ ((noipa)) static void					\
pack##N (_Float16 *__restrict h, const float *__restrict f)		\
{									\
  for (int n = 0; n < N; ++n)						\
    h[n] = f[n];							\
}									\
__attribute__ ((noipa)) static void					\
unpack##N (float *__restrict f, const _Float16 *__restrict h)		\
{									\
  for (int n = 0; n < N; ++n)						\
    f[n] = h[n];							\
}

DEF (8)
DEF (16)
DEF (32)
DEF (128)

__attribute__ ((noipa)) static void
packv (_Float16 *__restrict h, const float *__restrict f, int cnt)
{
  for (int n = 0; n < cnt; ++n)
    h[n] = f[n];
}

__attribute__ ((noipa)) static void
unpackv (float *__restrict f, const _Float16 *__restrict h, int cnt)
{
  for (int n = 0; n < cnt; ++n)
    f[n] = h[n];
}

REF_ATTR static void
ref_pack (_Float16 *h, const float *f, int cnt)
{
  for (int n = 0; n < cnt; ++n)
    h[n] = f[n];
}

REF_ATTR static void
ref_unpack (float *f, const _Float16 *h, int cnt)
{
  for (int n = 0; n < cnt; ++n)
    f[n] = h[n];
}

static float fin[MAXN], fout[MAXN], fref[MAXN];
static _Float16 hin[MAXN], hout[MAXN], href[MAXN];

#define CHECK_PACK(CALL, CNT)						\
  do {									\
    memset (hout, 0x55, sizeof hout);					\
    memset (href, 0x55, sizeof href);					\
    CALL;								\
    ref_pack (href, fin, CNT);						\
    if (memcmp (hout, href, sizeof hout) != 0)				\
      abort ();								\
  } while (0)

#define CHECK_UNPACK(CALL, CNT)						\
  do {									\
    memset (fout, 0x55, sizeof fout);					\
    memset (fref, 0x55, sizeof fref);					\
    CALL;								\
    ref_unpack (fref, hin, CNT);					\
    if (memcmp (fout, fref, sizeof fout) != 0)				\
      abort ();								\
  } while (0)

static void
run_tests (void)
{
  /* Even lanes are exactly representable as _Float16, odd lanes need
     rounding.  Values differ from lane to lane so that any lane
     permutation is detected.  */
  for (int i = 0; i < MAXN; i++)
    {
      int v = (i * 7) % 251 - 100;
      fin[i] = (i & 1) ? v * 0.1f : v * 0.25f;
    }
  ref_pack (hin, fin, MAXN);

  CHECK_PACK (pack8 (hout, fin), 8);
  CHECK_PACK (pack16 (hout, fin), 16);
  CHECK_PACK (pack32 (hout, fin), 32);
  CHECK_PACK (pack128 (hout, fin), 128);

  CHECK_UNPACK (unpack8 (fout, hin), 8);
  CHECK_UNPACK (unpack16 (fout, hin), 16);
  CHECK_UNPACK (unpack32 (fout, hin), 32);
  CHECK_UNPACK (unpack128 (fout, hin), 128);

  /* Counts that are not a multiple of the vector length exercise the
     epilogue, which can use narrower vectors.  */
  static const int cnts[] = { 0, 1, 3, 5, 9, 17, 37, 100, 127 };
  for (unsigned int i = 0; i < sizeof cnts / sizeof cnts[0]; i++)
    {
      CHECK_PACK (packv (hout, fin, cnts[i]), cnts[i]);
      CHECK_UNPACK (unpackv (fout, hin, cnts[i]), cnts[i]);
    }
}
