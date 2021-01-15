/* Machine description pattern tests.  */

/* { dg-do compile } */
/* { dg-options "-lpthread" } */
/* { dg-do run { target { s390_useable_hw } } } */

/**/

char
ae_8_0 (char *lock)
{
  return __atomic_exchange_n (lock, 0, 2);
}

char
ae_8_1 (char *lock)
{
  return __atomic_exchange_n (lock, 1, 2);
}

char g8;

char
ae_8_g_0 (void)
{
  return __atomic_exchange_n (&g8, 0, 2);
}

char
ae_8_g_1 (void)
{
  return __atomic_exchange_n (&g8, 1, 2);
}

/**/

short
ae_16_0 (short *lock)
{
  return __atomic_exchange_n (lock, 0, 2);
}

short
ae_16_1 (short *lock)
{
  return __atomic_exchange_n (lock, 1, 2);
}

short g16;

short
ae_16_g_0 (void)
{
  return __atomic_exchange_n (&g16, 0, 2);
}

short
ae_16_g_1 (void)
{
  return __atomic_exchange_n (&g16, 1, 2);
}

/**/

int
ae_32_0 (int *lock)
{
  return __atomic_exchange_n (lock, 0, 2);
}

int
ae_32_1 (int *lock)
{
  return __atomic_exchange_n (lock, 1, 2);
}

int g32;

int
ae_32_g_0 (void)
{
  return __atomic_exchange_n (&g32, 0, 2);
}

int
ae_32_g_1 (void)
{
  return __atomic_exchange_n (&g32, 1, 2);
}

/**/

long long
ae_64_0 (long long *lock)
{
  return __atomic_exchange_n (lock, 0, 2);
}

long long
ae_64_1 (long long *lock)
{
  return __atomic_exchange_n (lock, 1, 2);
}

long long g64;

long long
 ae_64_g_0 (void)
{
  return __atomic_exchange_n (&g64, 0, 2);
}

long long
ae_64_g_1 (void)
{
  return __atomic_exchange_n (&g64, 1, 2);
}

/**/

#ifdef __s390x__
__int128
ae_128_0 (__int128 *lock)
{
  return __atomic_exchange_n (lock, 0, 2);
}

__int128
ae_128_1 (__int128 *lock)
{
  return __atomic_exchange_n (lock, 1, 2);
}

__int128 g128;

__int128
ae_128_g_0 (void)
{
  return __atomic_exchange_n (&g128, 0, 2);
}

__int128
ae_128_g_1 (void)
{
  return __atomic_exchange_n (&g128, 1, 2);
}

#endif

int main(void)
{
  int i;

  for (i = 0; i <= 2; i++)
    {
      int oval = i;

      {
	char lock;
	char rval;

	lock = oval;
	rval = ae_8_0 (&lock);
	if (lock != 0)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	lock = oval;
	rval = ae_8_1 (&lock);
	if (lock != 1)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	g8 = oval;
	rval = ae_8_g_0 ();
	if (g8 != 0)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	g8 = oval;
	rval = ae_8_g_1 ();
	if (g8 != 1)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
      }
      {
	short lock;
	short rval;

	lock = oval;
	rval = ae_16_0 (&lock);
	if (lock != 0)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	lock = oval;
	rval = ae_16_1 (&lock);
	if (lock != 1)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	g16 = oval;
	rval = ae_16_g_0 ();
	if (g16 != 0)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	g16 = oval;
	rval = ae_16_g_1 ();
	if (g16 != 1)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
      }
      {
	int lock;
	int rval;

	lock = oval;
	rval = ae_32_0 (&lock);
	if (lock != 0)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	lock = oval;
	rval = ae_32_1 (&lock);
	if (lock != 1)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	g32 = oval;
	rval = ae_32_g_0 ();
	if (g32 != 0)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	g32 = oval;
	rval = ae_32_g_1 ();
	if (g32 != 1)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
      }
      {
	long long lock;
	long long rval;

	lock = oval;
	rval = ae_64_0 (&lock);
	if (lock != 0)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	lock = oval;
	rval = ae_64_1 (&lock);
	if (lock != 1)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	g64 = oval;
	rval = ae_64_g_0 ();
	if (g64 != 0)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	g64 = oval;
	rval = ae_64_g_1 ();
	if (g64 != 1)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
      }

#ifdef __s390x__
      {
	__int128 lock;
	__int128 rval;

	lock = oval;
	rval = ae_128_0 (&lock);
	if (lock != 0)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	lock = oval;
	rval = ae_128_1 (&lock);
	if (lock != 1)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	g128 = oval;
	rval = ae_128_g_0 ();
	if (g128 != 0)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
	g128 = oval;
	rval = ae_128_g_1 ();
	if (g128 != 1)
	  __builtin_abort ();
	if (rval != oval)
	  __builtin_abort ();
      }
#endif
    }

  return 0;
}
