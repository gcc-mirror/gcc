#include "avx2-check.h"

#define SIZE 256

TYPE a[SIZE];
TYPE b[SIZE];
TYPE c[SIZE];
volatile TYPE c_ref[SIZE];

__attribute__ ((__noinline__))
void
gen_pop ()
{
  int i;
  for (i = 0; i < SIZE; ++i)
#ifdef BIN_OP
    c[i] = BIN_OP (a[i], b[i]);
#else /*  Must be UN_OP */
    c[i] = UN_OP (a[i]);
#endif /*  BIN_OP */
}

void
check_pop ()
{
  int i;
  for (i = 0; i < SIZE; ++i)
#ifdef BIN_OP
    c_ref[i] = BIN_OP (a[i], b[i]);
#else /*  Must be UN_OP */
    c_ref[i] = UN_OP (a[i]);
#endif /*  BIN_OP */
}

void static
avx2_test (void)
{
  int i, j;
  for (i = 0; i < 4; ++i )
    {
      for ( j = 0; j < SIZE; ++j )
	{
	  a[i] = i * i + i;
	  b[i] = i * i * i;
	}

      gen_pop ();
      check_pop ();

      /* We need to cast away volatility from c_ref here in order to eliminate
	 warning if libc version of memcpy is used here.  */
      if (memcmp (c, (void *) c_ref, SIZE * sizeof (TYPE)))
	abort();
    }
}
