/* Funtional setmem test.  */

/* { dg-do run } */
/* { dg-options "-O3" } */

#define MAX_LEN (8 * 1000)

__attribute__ ((noinline))
int
check_mem (char *mem, int val, int len)
{
  int i;

  if (mem[0] != 0x71)
    __builtin_abort();
  for (i = 1; i <= len; i++)
    if (mem[i] != val)
      __builtin_abort();
  if (mem[len + 1] != 0x71 + (len + 1) % 4)
    __builtin_abort();
}

__attribute__ ((noinline))
void
init_mem (char *mem)
{
  unsigned int *p;
  int i;

  p = (unsigned int *)mem;
  for (i = 0; i < MAX_LEN / sizeof(unsigned int); i++)
    p[i] = 0x71727374;
}

#define MEMSET_CHECK(VAL, SIZE)			\
  init_mem (mem1);				\
  __builtin_memset (mem1 + 1, 0, (SIZE));	\
  check_mem (mem1, 0, SIZE);			\
  init_mem (mem2);				\
  __builtin_memset (mem2 + 1, (VAL), (SIZE));	\
  check_mem (mem2, VAL, SIZE);

char mem1[MAX_LEN + 2];
char mem2[MAX_LEN + 2];

int main(int argc, char **argv)
{
  int lens[] =
    {
      256, 257, 258, 259,
      512, 513, 514, 515,
      768, 769, 770, 771,
      1024, 1025, 1026, 1027,
      1280, 1281, 1282, 1283,
      -999
    };
  int t;

  /* variable length */
  for (t = 0; lens[t] != -999; t++)
    {
      MEMSET_CHECK (argc + 0x10, lens[t]);
    }

  /* constant length */
  MEMSET_CHECK (argc + 0x10, 0);
  MEMSET_CHECK (argc + 0x10, 1);
  MEMSET_CHECK (argc + 0x10, 2);
  MEMSET_CHECK (argc + 0x10, 3);
  MEMSET_CHECK (argc + 0x10, 256);
  MEMSET_CHECK (argc + 0x10, 257);
  MEMSET_CHECK (argc + 0x10, 258);
  MEMSET_CHECK (argc + 0x10, 259);
  MEMSET_CHECK (argc + 0x10, 512);
  MEMSET_CHECK (argc + 0x10, 513);
  MEMSET_CHECK (argc + 0x10, 514);
  MEMSET_CHECK (argc + 0x10, 515);
  MEMSET_CHECK (argc + 0x10, 768);
  MEMSET_CHECK (argc + 0x10, 769);
  MEMSET_CHECK (argc + 0x10, 770);
  MEMSET_CHECK (argc + 0x10, 771);
  MEMSET_CHECK (argc + 0x10, 1024);
  MEMSET_CHECK (argc + 0x10, 1025);
  MEMSET_CHECK (argc + 0x10, 1026);
  MEMSET_CHECK (argc + 0x10, 1027);
  MEMSET_CHECK (argc + 0x10, 1280);
  MEMSET_CHECK (argc + 0x10, 1281);
  MEMSET_CHECK (argc + 0x10, 1282);
  MEMSET_CHECK (argc + 0x10, 1283);

  return 0;
}
