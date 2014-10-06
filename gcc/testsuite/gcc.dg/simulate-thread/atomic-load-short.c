/* { dg-do link } */
/* { dg-require-effective-target sync_char_short } */
/* { dg-final { simulate-thread } } */


#include <stdio.h>
#include "simulate-thread.h"


/* Testing load for atomicity is a little trickier.  

   Set up the atomic value so that it changes value after every instruction 
   is executed.

   Simply alternating between 2 values wouldn't be sufficient since a load of
   one part, followed by the load of the second part 2 instructions later would
   appear to be valid.

   set up a table of 16 values which change a bit in every byte of the value 
   each time, this will give us a 16 instruction cycle before repetition
   kicks in, which should be sufficient to detect any issues.  Just to be sure,
   we also change the table cycle size during execution. 
   
   The end result is that all loads should always get one of the values from
   the table. Any other pattern means the load failed.  */

unsigned short ret;
unsigned short value = 0;
unsigned short result = 0;
unsigned short table[16] = {
0x0000, 
0x1111, 
0x2222, 
0x3333,
0x4444,
0x5555,
0x6666,
0x7777,
0x8888,
0x9999,
0xAAAA,
0xBBBB,
0xCCCC,
0xDDDD,
0xEEEE,
0xFFFF
};

int table_cycle_size = 16;

/* Return 0 if 'result' is a valid value to have loaded.  */
int verify_result ()
{
  int x;
  int found = 0;

  /* Check entire table for valid values.  */
  for (x = 0; x < 16 ; x++)
    if (result == table[x])
      {
	found = 1;
	break;
      }

  if (!found)
    printf("FAIL: Invalid result returned from fetch\n");

  return !found;
}

/* Iterate VALUE through the different valid values. */
void simulate_thread_other_threads ()
{
  static int current = 0;

  if (++current >= table_cycle_size)
    current = 0;
  value = table[current];
}

int simulate_thread_step_verify ()
{
  return verify_result ();
}

int simulate_thread_final_verify ()
{
  return verify_result ();
}

__attribute__((noinline))
void simulate_thread_main()
{
  int x;

  /* Execute loads with value changing at various cyclic values.  */
  for (table_cycle_size = 16; table_cycle_size > 4 ; table_cycle_size--)
    {
      ret = __atomic_load_n (&value, __ATOMIC_SEQ_CST);
      /* In order to verify the returned value (which is not atomic), it needs
	 to be atomically stored into another variable and check that.  */
      __atomic_store_n (&result, ret, __ATOMIC_SEQ_CST);

      /* Execute the fetch/store a couple of times just to ensure the cycles
         have a chance to be interesting.  */
      ret = __atomic_load_n (&value, __ATOMIC_SEQ_CST);
      __atomic_store_n (&result, ret, __ATOMIC_SEQ_CST);
    }
}

int
main()
{
  simulate_thread_main ();
  simulate_thread_done ();
  return 0;
}
