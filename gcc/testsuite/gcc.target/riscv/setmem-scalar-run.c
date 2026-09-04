/* { dg-do run } */
/* { dg-additional-options "-O2 -mtune=generic-ooo -mstringop-strategy=scalar" } */

#define EMPTY 16
#define SZ 128
#define FILL 0xa5

#define FOR_EACH_LEN(M)						\
  M(1) M(2) M(3) M(4) M(5) M(7) M(8) M(9) M(15) M(16)		\
  M(17) M(23) M(24) M(25) M(31) M(32) M(33) M(40) M(63)		\
  M(64) M(65) M(71) M(96) M(127) M(128)

#define DEF(N)							\
  __attribute__((noinline)) static void				\
  setmem_##N (void *dst, int c)					\
  {								\
    __builtin_memset (dst, c, N);				\
  }

FOR_EACH_LEN (DEF)

static unsigned char buf[EMPTY + SZ + EMPTY];

static void
test (unsigned off, unsigned len, int c)
{
  unsigned i;

  for (i = 0; i < sizeof (buf); i++)
    {
      unsigned char want = (i >= off && i < off + len)
			   ? (unsigned char) c : FILL;
      if (buf[i] != want)
	__builtin_abort ();
    }
}

#define RUN(N)							\
  __builtin_memset (buf, FILL, sizeof (buf));			\
  setmem_##N (buf + EMPTY + 1, c);				\
  test (EMPTY + 1, N, c);

static void
run (int c)
{
  FOR_EACH_LEN (RUN)
}

int
main (void)
{
  run (0);
  run (1);
  run (3);
  run (0x5a);
  run (0xff);
  return 0;
}
