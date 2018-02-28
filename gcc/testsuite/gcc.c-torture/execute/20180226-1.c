/* PR rtl-optimization/83496 */
/* Reported by Hauke Mehrtens <gcc@hauke-m.de> */

extern void abort (void);

typedef unsigned long mp_digit;

typedef struct { int used, alloc, sign; mp_digit *dp; } mp_int;

int mytest(mp_int *a, mp_digit b) __attribute__((noclone, noinline));

int mytest(mp_int *a, mp_digit b)
{
  if (a->sign == 1)
    return -1;
  if (a->used > 1)
    return 1;
  if (a->dp[0] > b)
    return 1;
  if (a->dp[0] < b)
    return -1;
  return 0;
}

int main (void)
{
  mp_int i = { 2, 0, -1 };
  if (mytest (&i, 0) != 1)
    abort ();
  return 0;
}
