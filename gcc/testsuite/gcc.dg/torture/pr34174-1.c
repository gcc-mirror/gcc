/* { dg-do run } */
/* Based on PR target/27386 testcase by Joerg Wunsch.  */

extern void abort (void);
extern void exit (int);

#if __INT_MAX__ >= 9223372036854775807LL
typedef unsigned int uint64_t;
#elif __LONG_MAX__ >= 9223372036854775807LL
typedef unsigned long int uint64_t;
#elif __LONG_LONG_MAX__ >= 9223372036854775807LL
typedef unsigned long long int uint64_t;
#else
int
main (void)
{
  exit (0);
}
#endif

uint64_t a, b, c;

int
foo (uint64_t x, uint64_t y, uint64_t z, int i)
{
  a = x;
  b = y;
  c = z;
  return 2 * i;
}

int
main (void)
{
  if (foo (1234512345123ull, 3456734567345ull, 7897897897897ull, 42) != 84)
    abort ();
  if (a != 1234512345123ull)
    abort ();
  if (b != 3456734567345ull)
    abort ();
  if (c != 7897897897897ull)
    abort ();
  exit (0);
}
