/* { dg-do run } */
/* { dg-additional-options "-O2 -mtune=generic-ooo -mstringop-strategy=scalar" } */

#define ALIGN 8
#define SZ 160
#define EMPTY 32

#define ARR (2 * SZ + 2 * ALIGN)
#define BUFSZ (EMPTY + ARR + EMPTY)

#define FOR_EACH_LEN(M)						\
  M(1) M(2) M(3) M(4) M(5) M(6) M(7) M(8) M(9) M(15) M(16)	\
  M(17) M(23) M(24) M(25) M(31) M(32) M(33) M(40) M(47)		\
  M(48) M(49) M(63) M(64) M(65) M(95) M(96) M(97) M(128) M(160)

#define DEF(N)							\
  __attribute__((noinline)) static void				\
  move_##N (void *dst, void *src)				\
  {								\
    __builtin_memmove (dst, src, N);				\
  }

FOR_EACH_LEN (DEF)

static unsigned char arr[BUFSZ];
static unsigned char ref[BUFSZ];
static unsigned char tmp[SZ];

static void
init (void)
{
  unsigned i;

  for (i = 0; i < BUFSZ; i++)
    arr[i] = ref[i] = (unsigned char) (i * 7 + 1);
}

static void
ref_move (unsigned off1, unsigned off2, unsigned len)
{
  unsigned i;

  for (i = 0; i < len; i++)
    tmp[i] = ref[off2 + i];
  for (i = 0; i < len; i++)
    ref[off1 + i] = tmp[i];
}

typedef void (*fun) (void *, void *);

static void
test (fun f, unsigned len, unsigned off1, unsigned off2)
{
  unsigned i;

  init ();
  ref_move (off1, off2, len);
  f (arr + off1, arr + off2);

  for (i = 0; i < BUFSZ; i++)
    if (arr[i] != ref[i])
      __builtin_abort ();
}

static void
test_len (fun f, unsigned len)
{
  unsigned deltas[6];
  unsigned delta, di, si, k;

  for (delta = 0; delta <= len + ALIGN; delta++)
    {
      test (f, len, EMPTY, EMPTY + delta);
      test (f, len, EMPTY + delta, EMPTY);
    }

  deltas[0] = 0;
  deltas[1] = 1;
  deltas[2] = len / 2;
  deltas[3] = len > 0 ? len - 1 : 0;
  deltas[4] = len;
  deltas[5] = len + 1;

  for (di = 0; di < ALIGN; di++)
    for (si = 0; si < ALIGN; si++)
      for (k = 0; k < 6; k++)
	{
	  test (f, len, EMPTY + di, EMPTY + si + deltas[k]);
	  test (f, len, EMPTY + di + deltas[k], EMPTY + si);
	}
}

#define RUN(N) test_len (move_##N, N);

int
main (void)
{
  FOR_EACH_LEN (RUN)
  return 0;
}
