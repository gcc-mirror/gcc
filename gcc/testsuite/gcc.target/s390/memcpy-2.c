/* Funtional memmov test.  */

/* { dg-do run } */
/* { dg-options "-O3" } */

#define MAX_LEN (8 * 1000)
#define X 0x11

char gsrc[MAX_LEN + 2];
char gdst[MAX_LEN + 2];

__attribute__ ((noinline))
int
compare_mem (int len)
{
  int i;

  if (gdst[0] != 0x61)
    __builtin_abort();
  for (i = 1; i <= len; i++)
    if (gsrc[i] != gdst[i])
      __builtin_abort();
  for (i = len + 1; i < MAX_LEN; i++)
    if (gdst[i] != 0x61 + i % 4)
      __builtin_abort();
}

__attribute__ ((noinline))
void
init_mem (void)
{
  unsigned int *p1;
  unsigned int *p2;
  int i;

  p1 = (unsigned int *)gsrc;
  p2 = (unsigned int *)gdst;
  for (i = 0; i < MAX_LEN / sizeof(unsigned int); i++)
    {
      p1[i] = 0x71727374;
      p2[i] = 0x61626364;
    }
}

#define MEMCPY_CHECK(DST, SRC, LEN)			\
  init_mem ();						\
  __builtin_memcpy ((DST) + 1, (SRC) + 1, (LEN));	\
  compare_mem ((LEN));


int main(void)
{
  int lens[] =
    {
      255, 256, 257,
      511, 512, 513,
      767, 768, 769,
      1023, 1024, 1025,
      1279, 1280, 1281,
      1535, 1536, 1537,
      -999
    };
  int t;

  /* variable length */
  for (t = 0; lens[t] != -999; t++)
    {
      MEMCPY_CHECK (gdst, gsrc, lens[t]);
    }
  /* constant length */
  MEMCPY_CHECK (gdst, gsrc, 0);
  MEMCPY_CHECK (gdst, gsrc, 1);
  MEMCPY_CHECK (gdst, gsrc, 2);
  MEMCPY_CHECK (gdst, gsrc, 3);
  MEMCPY_CHECK (gdst, gsrc, 256);
  MEMCPY_CHECK (gdst, gsrc, 257);
  MEMCPY_CHECK (gdst, gsrc, 511);
  MEMCPY_CHECK (gdst, gsrc, 512);
  MEMCPY_CHECK (gdst, gsrc, 513);
  MEMCPY_CHECK (gdst, gsrc, 767);
  MEMCPY_CHECK (gdst, gsrc, 768);
  MEMCPY_CHECK (gdst, gsrc, 769);
  MEMCPY_CHECK (gdst, gsrc, 1023);
  MEMCPY_CHECK (gdst, gsrc, 1024);
  MEMCPY_CHECK (gdst, gsrc, 1025);
  MEMCPY_CHECK (gdst, gsrc, 1279);
  MEMCPY_CHECK (gdst, gsrc, 1280);
  MEMCPY_CHECK (gdst, gsrc, 1281);
  MEMCPY_CHECK (gdst, gsrc, 1535);
  MEMCPY_CHECK (gdst, gsrc, 1536);
  MEMCPY_CHECK (gdst, gsrc, 1537);

  return 0;
}
