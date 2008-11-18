typedef __SIZE_TYPE__ size_t;
static int mymemcmp1 (unsigned long int, unsigned long int)
  __attribute__ ((__nothrow__));

__inline static int
mymemcmp1 (unsigned long int a, unsigned long int b)
{
  long int srcp1 = (long int) &a;
  long int srcp2 = (long int) &b;
  unsigned long int a0, b0;
  do
    {
      a0 = ((unsigned char *) srcp1)[0];
      b0 = ((unsigned char *) srcp2)[0];
      srcp1 += 1;
      srcp2 += 1;
    }
  while (a0 == b0);
  return a0 - b0;
}

static int mymemcmp2 (long, long, size_t) __attribute__ ((__nothrow__));

static int
mymemcmp2 (long int srcp1, long int srcp2, size_t len)
{
  unsigned long int a0, a1;
  unsigned long int b0, b1;
  switch (len % 4)
    {
    default:
    case 2:
      a0 = ((unsigned long int *) srcp1)[0];
      b0 = ((unsigned long int *) srcp2)[0];
      srcp1 -= 2 * (sizeof (unsigned long int));
      srcp2 -= 2 * (sizeof (unsigned long int));
      len += 2;
      goto do1;
    case 3:
      a1 = ((unsigned long int *) srcp1)[0];
      b1 = ((unsigned long int *) srcp2)[0];
      srcp1 -= (sizeof (unsigned long int));
      srcp2 -= (sizeof (unsigned long int));
      len += 1;
      goto do2;
    case 0:
      if (16 <= 3 * (sizeof (unsigned long int)) && len == 0)
        return 0;
      a0 = ((unsigned long int *) srcp1)[0];
      b0 = ((unsigned long int *) srcp2)[0];
      goto do3;
    case 1:
      a1 = ((unsigned long int *) srcp1)[0];
      b1 = ((unsigned long int *) srcp2)[0];
      srcp1 += (sizeof (unsigned long int));
      srcp2 += (sizeof (unsigned long int));
      len -= 1;
      if (16 <= 3 * (sizeof (unsigned long int)) && len == 0)
        goto do0;
    }
  do
    {
      a0 = ((unsigned long int *) srcp1)[0];
      b0 = ((unsigned long int *) srcp2)[0];
      if (a1 != b1)
        return mymemcmp1 ((a1), (b1));
    do3:
      a1 = ((unsigned long int *) srcp1)[1];
      b1 = ((unsigned long int *) srcp2)[1];
      if (a0 != b0)
        return mymemcmp1 ((a0), (b0));
    do2:
      a0 = ((unsigned long int *) srcp1)[2];
      b0 = ((unsigned long int *) srcp2)[2];
      if (a1 != b1)
        return mymemcmp1 ((a1), (b1));
    do1:
      a1 = ((unsigned long int *) srcp1)[3];
      b1 = ((unsigned long int *) srcp2)[3];
      if (a0 != b0)
        return mymemcmp1 ((a0), (b0));
      srcp1 += 4 * (sizeof (unsigned long int));
      srcp2 += 4 * (sizeof (unsigned long int));
      len -= 4;
    }
  while (len != 0);
do0:
  if (a1 != b1)
    return mymemcmp1 ((a1), (b1));
  return 0;
}

static int mymemcmp3 (long, long, size_t) __attribute__ ((__nothrow__));

static int
mymemcmp3 (long int srcp1, long int srcp2, size_t len)
{
  unsigned long int a0, a1, a2, a3;
  unsigned long int b0, b1, b2, b3;
  unsigned long int x;
  int shl, shr;
  shl = 8 * (srcp1 % (sizeof (unsigned long int)));
  shr = 8 * (sizeof (unsigned long int)) - shl;
  srcp1 &= -(sizeof (unsigned long int));
  switch (len % 4)
    {
    default:
    case 2:
      a1 = ((unsigned long int *) srcp1)[0];
      a2 = ((unsigned long int *) srcp1)[1];
      b2 = ((unsigned long int *) srcp2)[0];
      srcp1 -= 1 * (sizeof (unsigned long int));
      srcp2 -= 2 * (sizeof (unsigned long int));
      len += 2;
      goto do1;
    case 3:
      a0 = ((unsigned long int *) srcp1)[0];
      a1 = ((unsigned long int *) srcp1)[1];
      b1 = ((unsigned long int *) srcp2)[0];
      srcp2 -= 1 * (sizeof (unsigned long int));
      len += 1;
      goto do2;
    case 0:
      if (16 <= 3 * (sizeof (unsigned long int)) && len == 0)
        return 0;
      a3 = ((unsigned long int *) srcp1)[0];
      a0 = ((unsigned long int *) srcp1)[1];
      b0 = ((unsigned long int *) srcp2)[0];
      srcp1 += 1 * (sizeof (unsigned long int));
      goto do3;
    case 1:
      a2 = ((unsigned long int *) srcp1)[0];
      a3 = ((unsigned long int *) srcp1)[1];
      b3 = ((unsigned long int *) srcp2)[0];
      srcp1 += 2 * (sizeof (unsigned long int));
      srcp2 += 1 * (sizeof (unsigned long int));
      len -= 1;
      if (16 <= 3 * (sizeof (unsigned long int)) && len == 0)
        goto do0;
    }
  do
    {
      a0 = ((unsigned long int *) srcp1)[0];
      b0 = ((unsigned long int *) srcp2)[0];
      x = (((a2) >> (shl)) | ((a3) << (shr)));
      if (x != b3)
        return mymemcmp1 ((x), (b3));
    do3:
      a1 = ((unsigned long int *) srcp1)[1];
      b1 = ((unsigned long int *) srcp2)[1];
      x = (((a3) >> (shl)) | ((a0) << (shr)));
      if (x != b0)
        return mymemcmp1 ((x), (b0));
    do2:
      a2 = ((unsigned long int *) srcp1)[2];
      b2 = ((unsigned long int *) srcp2)[2];
      x = (((a0) >> (shl)) | ((a1) << (shr)));
      if (x != b1)
        return mymemcmp1 ((x), (b1));
    do1:
      a3 = ((unsigned long int *) srcp1)[3];
      b3 = ((unsigned long int *) srcp2)[3];
      x = (((a1) >> (shl)) | ((a2) << (shr)));
      if (x != b2)
        return mymemcmp1 ((x), (b2));
      srcp1 += 4 * (sizeof (unsigned long int));
      srcp2 += 4 * (sizeof (unsigned long int));
      len -= 4;
    }
  while (len != 0);
do0:
  x = (((a2) >> (shl)) | ((a3) << (shr)));
  if (x != b3)
    return mymemcmp1 ((x), (b3));
  return 0;
}

__attribute__ ((noinline))
int mymemcmp (const void *s1, const void *s2, size_t len)
{
  unsigned long int a0;
  unsigned long int b0;
  long int srcp1 = (long int) s1;
  long int srcp2 = (long int) s2;
  if (srcp1 % (sizeof (unsigned long int)) == 0)
    return mymemcmp2 (srcp1, srcp2, len / (sizeof (unsigned long int)));
  else
    return mymemcmp3 (srcp1, srcp2, len / (sizeof (unsigned long int)));
}

char buf[256];

int
main (void)
{
  char *p;
  union { long int l; char c[sizeof (long int)]; } u;

  /* The test above assumes little endian and long being the same size
     as pointer.  */
  if (sizeof (long int) != sizeof (void *) || sizeof (long int) < 4)
    return 0;
  u.l = 0x12345678L;
  if (u.c[0] != 0x78 || u.c[1] != 0x56 || u.c[2] != 0x34 || u.c[3] != 0x12)
    return 0;

  p = buf + 16 - (((long int) buf) & 15);
  __builtin_memcpy (p + 9,
"\x1\x37\x82\xa7\x55\x49\x9d\xbf\xf8\x44\xb6\x55\x17\x8e\xf9", 15);
  __builtin_memcpy (p + 128 + 24,
"\x1\x37\x82\xa7\x55\x49\xd0\xf3\xb7\x2a\x6d\x23\x71\x49\x6a", 15);
  if (mymemcmp (p + 9, p + 128 + 24, 33) != -51)
    __builtin_abort ();
  return 0;
}
