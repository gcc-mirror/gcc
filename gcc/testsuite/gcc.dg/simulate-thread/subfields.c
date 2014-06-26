/* { dg-do link } */
/* { dg-final { simulate-thread } } */

#include <stdio.h>
#include "simulate-thread.h"

/* This test verifies that data races aren't introduced by structure subfield 
   stores. */

struct test_struct {
  char a;
  char b;
  char c;
  char d;
} var = {0,0,0,0};


/* This routine sets field a to 'x'.  If executed properly, it will
   not affect any of the other fields in the structure.  An improper
   implementation may load an entire word, change the 8 bits for field
   'a' and write the entire word back out. */
__attribute__((noinline))
void set_a(char x)
{
  var.a = x;
}

static int global = 0;

/* The other thread increments the value of each of the other fields
   in the structure every cycle.  If the store to the 'a' field does
   an incorrect full or partial word load, mask and store, it will
   write back an incorrect value to one or more of the other
   fields.  */
void simulate_thread_other_threads() 
{
  global++;
  var.b = global;
  var.c = global;
  var.d = global;
}


/* Make sure that none of the other fields have been changed.  */
int simulate_thread_step_verify()
{
  int ret = 0;
  if (var.b != global)
    {
      printf("FAIL: Unexpected value. var.b is %d, should be %d\n",
	     var.b, global);
      ret = 1;
    }
  if (var.c != global)
    {
      printf("FAIL: Unexpected value. var.c is %d, should be %d\n",
	     var.c, global);
      ret = 1;
    }
  if (var.d != global)
    {
      printf("FAIL: Unexpected value. var.d is %d, should be %d\n",
	     var.d, global);
      ret = 1;
    }
  return ret;
}

/* Verify that every variable has the correct value.  */
int simulate_thread_final_verify()
{
  int ret = simulate_thread_step_verify();
  if (var.a != 1)
    {
      printf("FAIL: Unexpected value. var.a is %d, should be %d\n", var.a, 1);
      ret = 1;
    }
  return ret;
}

__attribute__((noinline))
void simulate_thread_main()
{
  set_a(1);
}

int main ()
{
  simulate_thread_main();
  simulate_thread_done();
  return 0;
}
