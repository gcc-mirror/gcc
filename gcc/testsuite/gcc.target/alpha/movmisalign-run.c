/* Verify that misaligned loads and stores of all sizes produce correct
   results at every alignment.  */

/* { dg-do run } */
/* { dg-options "" } */

static unsigned char buf[64];
static unsigned char ref[64];
static unsigned char exp[64];

#define DEFINE_ACCESSORS(SUFFIX, TYPE)					\
  __attribute__ ((noipa)) static TYPE					\
  load_##SUFFIX (const unsigned char *p)				\
  {									\
    TYPE v;								\
									\
    __builtin_memcpy (&v, p, sizeof v);					\
    return v;								\
  }									\
									\
  __attribute__ ((noipa)) static void					\
  store_##SUFFIX (unsigned char *p, TYPE v)				\
  {									\
    __builtin_memcpy (p, &v, sizeof v);					\
  }

DEFINE_ACCESSORS (hi, unsigned short)
DEFINE_ACCESSORS (si, unsigned int)
DEFINE_ACCESSORS (di, unsigned long)

#define CHECK_LOAD(SUFFIX, TYPE, OFS)					\
  do									\
    {									\
      TYPE v;								\
									\
      __builtin_memcpy (&v, ref + (OFS), sizeof v);			\
      if (load_##SUFFIX (buf + (OFS)) != v)				\
	__builtin_abort ();						\
    }									\
  while (0)

#define CHECK_STORE(SUFFIX, TYPE, OFS, VAL)				\
  do									\
    {									\
      TYPE v = (VAL);							\
									\
      __builtin_memcpy (buf, ref, sizeof buf);				\
      __builtin_memcpy (exp, ref, sizeof exp);				\
      store_##SUFFIX (buf + (OFS), v);					\
      __builtin_memcpy (exp + (OFS), &v, sizeof v);			\
      if (__builtin_memcmp (buf, exp, sizeof buf) != 0)			\
	__builtin_abort ();						\
    }									\
  while (0)

int
main (void)
{
  int i;

  for (i = 0; i < (int) sizeof ref; i++)
    ref[i] = buf[i] = i * 7 + 3;

  for (i = 0; i < 16; i++)
    {
      CHECK_LOAD (hi, unsigned short, i);
      CHECK_LOAD (si, unsigned int, i);
      CHECK_LOAD (di, unsigned long, i);
    }

  for (i = 0; i < 16; i++)
    {
      CHECK_STORE (hi, unsigned short, i, 0x1234);
      CHECK_STORE (si, unsigned int, i, 0xdeadbeef);
      CHECK_STORE (di, unsigned long, i, 0x0123456789abcdef);
    }

  return 0;
}
