/* Copyright (C) 2006 Free Software Foundation, Inc. */
/* Contributed by Carlos O'Donell on 2006-03-14 */

/* Test that GCC follows the SPARC 32-bit psABI with regards to
   structure return checking in a callee. When -mstd-struct-return 
   is specificed then gcc will emit code to skip the unimp insn. */ 

/* Origin: Carlos O'Donell <carlos@codesourcery.com> */
/* { dg-do run { target sparc*-*-solaris* sparc*-*-linux* sparc*-*-*bsd* } } */
/* { dg-options "-mstd-struct-return -fno-pie" } */
/* { dg-require-effective-target ilp32 } */
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

/* Local declaration of div_t structure */
struct mydiv_t {
  int rem;
  int quot;
};

/* Global check variable used by signal handler */
int check = 1;
struct mydiv_t dcheck;

struct mydiv_t foo (void)
{
  struct mydiv_t bar;
  bar.rem = 3;
  bar.quot = 4;
  return bar;
}

void handle_sigill (int signum)
{
  if (signum == SIGILL && check == 2)
    {
      /* We expected a SIGILL due to a mismatch in unimp size
	 and struct mydiv_t size */
      exit (0);
    }
  else
    abort ();
}

/* Implement 3 checks to validate SPARC 32-bit psABI callee 
   returns struct
   
   Test1: Save area is valid. unimp size is valid.
   Success: Save area modified correctly.
   Failure: Save area unmodified.

   Test2: Save area is valid. unimp size is invalid (invalid insn).
   Success: Save area unmodified. check == 2.
   Failure: Save area modified or check == 1.

   Test3: Save area is invalid. unimp size is invalid (invalid size).
   Success: Will raise a SIGILL. 
   Failure: SIGSEGV caused by write to invalid save area. */

int main (void)
{
  dcheck.rem = 1;
  dcheck.quot = 2;

  /*** Test1 ***/
  /* Insert a call, insert unimp by hand */
  __asm__ ("st %1, [ %%sp + 0x40 ]\n\t"
	   "call foo\n\t"
	   " nop\n\t"
	   "unimp %2\n\t" 
	   : "=m" (dcheck)
	   : "r" (&dcheck), "i" (sizeof(struct mydiv_t)) 
	   : "memory");

  /* If the caller doesn't adjust the return, then it crashes.
     Check the result too. */

  if ((dcheck.rem != 3) || (dcheck.quot !=4))
    abort ();
  

  /*** Test 2 ***/
  dcheck.rem = 1;
  dcheck.quot = 2;

  /* Ignore the return of the function */
  __asm__ ("st %3, [ %%sp + 0x40 ]\n\t"
	   "call foo\n\t"
	   " nop\n\t"
	   "mov %2, %0\n\t"
	   : "+r" (check), "=m" (dcheck) 
	   : "i" (0x2), "r" (&dcheck)
	   : "memory");

  /* If the caller does an unconditional adjustment it will skip
     the mov, and then we can fail the test based on check's value 
     We pass a valid pointer to a save area in order to check if 
     caller incorrectly wrote to the save area as well. There may
     be a case where the unimp check and skip is correct, but the
     write to the save area still occurs. */

  if (check != 2)
    abort ();

  if ((dcheck.rem != 1) || (dcheck.quot != 2))
    abort ();

  /*** Test 3 ***/
  /* Prepare a test that must SIGILL. According to the spec
     if the sizes of the save area and return don't match then
     the copy is ignored and we return to the unimp. */

  signal (SIGILL, handle_sigill);

  __asm__ ("st %%g0, [ %%sp + 0x40 ]\n\t"
	   "call foo\n\t"
	   " nop\n\t"
	   "unimp %0\n\t"
	   : /* No outputs */ 
	   : "i" (sizeof(struct mydiv_t)-1) 
	   : "memory");

  /* NEVER REACHED */
  exit (0);
}
