#include "sse4_2-check.h"

#include <nmmintrin.h>

#define NUM 1024

static int
compute_popcnt (TYPE v)
{
  int ret;
  int i;

 ret = 0;
 for (i = 0; i < sizeof(v) * 8; i++)
   if ((v & ((TYPE)1 << (TYPE) i)))
     ret++;

 return ret;
}

static void
sse4_2_test (void)
{
  int i;
  TYPE vals[NUM];
  TYPE res;

 for (i = 0; i < NUM; i++)
   {
     vals[i] = rand ();
     if (sizeof (TYPE) > 4)
       vals[i] |= (TYPE)rand() << (TYPE)(sizeof (TYPE) * 4);
   }

 for (i=0; i < NUM; i++)
   { 
     res = POPCNT (vals[i]);
     if (res != compute_popcnt (vals[i]))
       abort ();
   }
}
