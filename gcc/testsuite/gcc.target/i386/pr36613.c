/* { dg-do run { target { { i?86-*-linux* x86_64-*-linux* } && ilp32 } } } */
/* { dg-options "-Os" } */
/* PR target/36613 */

extern void abort (void);

static inline int
lshifts (int val, int cnt)
{
  if (val < 0)
    return val;
  return val << cnt;
}

static inline unsigned int
lshiftu (unsigned int val, unsigned int cnt)
{
  if (cnt >= sizeof (unsigned int) * __CHAR_BIT__
      || val > ((__INT_MAX__ * 2U) >> cnt))
    return val;
  return val << cnt;
}

static inline int
rshifts (int val, unsigned int cnt)
{
  if (val < 0 || cnt >= sizeof (int) * __CHAR_BIT__)
    return val;
  return val >> cnt;
}

int
foo (unsigned int val)
{
  return rshifts (1 + val, lshifts (lshiftu (val, val), 1));
}

int
main (void)
{
  if (foo (1) != 0)
    abort ();
  return 0;
}
