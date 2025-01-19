/* { dg-do run } */
/* { dg-additional-options -std=gnu99 } */

#define NI __attribute__((__noipa__))
#define AI static inline __attribute__((__always_inline__))

/*****************************************************************************/

AI unsigned fn_crc_A (unsigned x, unsigned y)
{
  for (char i = 8; i--; x <<= 1)
    y ^= (x ^ y) & 0x80 ? 79U : 0U;
  return y;
}

NI unsigned fn_crc_N (unsigned x, unsigned y)
{
  for (char i = 8; i--; x <<= 1)
    y ^= (x ^ y) & 0x80 ? 79U : 0U;
  return y;
}

AI void test1_crc (unsigned x, unsigned y, int line)
{
  if (fn_crc_A (x, y) != fn_crc_N (x, y))
    __builtin_exit (line);
}

__attribute__((__optimize__(3)))
void test_crc (void)
{
  test1_crc (0x1ff, 0x1ff, __LINE__);
  test1_crc (0x1ab, 0x1cd, __LINE__);
  test1_crc (0xab, 0xcd, __LINE__);
  test1_crc (0x87, 0x65, __LINE__);
  test1_crc (0x3f, 0xb7, __LINE__);
}

/*****************************************************************************/

AI long fn_build4_A (char a, char b, char c, char d)
{
    long la = a;
    long lb = (long) b << 8;
    long lc = (long) c << 16;
    long ld = (long) b << 24;
    long x = (la & 0xff) | (lb & 0xff00) | (lc & 0xff0000) | (ld & 0xff000000);
    return x;
}

NI long fn_build4_N (char a, char b, char c, char d)
{
    long la = a;
    long lb = (long) b << 8;
    long lc = (long) c << 16;
    long ld = (long) b << 24;
    long x = (la & 0xff) | (lb & 0xff00) | (lc & 0xff0000) | (ld & 0xff000000);
    return x;
}

AI void test1_build4 (char a, char b, char c, char d, int line)
{
  if (fn_build4_A (a, b, c, d) != fn_build4_N (a, b, c, d))
    __builtin_exit (line);
}

void test_build4 (void)
{
  test1_build4 (1, 2, 3, 4, __LINE__);
  test1_build4 (-2, -3, -4, -5, __LINE__);
  test1_build4 (1, -2, 3, -4, __LINE__);
  test1_build4 (-1, 2, -3, 4, __LINE__);
}

/*****************************************************************************/

int main (void)
{
  test_crc ();
  test_build4 ();

  return 0;
}
