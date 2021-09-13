/* { dg-do assemble { target x86_64-*-* } } */
/* { dg-require-effective-target lp64 } */

#include "analyzer-decls.h"

#include <stdint.h>

int test_1 (int src)
{
  int dst;
  asm ("mov %1, %0\n\t"
       "add $1, %0"
       : "=r" (dst)
       : "r" (src));
  return dst;
}

uint32_t test_2 (uint32_t Mask)
{
  uint32_t Index;
  asm ("bsfl %[aMask], %[aIndex]"
       : [aIndex] "=r" (Index)
       : [aMask] "r" (Mask)
       : "cc");
  return Index;
}

int test_3a (int p1, int p2)
{
  asm goto ("btl %1, %0\n\t"
	    "jc %l2"
	    : // No outputs
	    : "r" (p1), "r" (p2)
	    : "cc"
	    : carry);

  return 0;

 carry:
  return 1;
}

int test_3b (int p1, int p2)
{
  asm goto ("btl %1, %0\n\t"
	    "jc %l[carry]"
	    : // No outputs
	    : "r" (p1), "r" (p2)
	    : "cc"
	    : carry);

  return 0;

 carry:
  return 1;
}

uint64_t test_4 (void)
{
  uint64_t start_time, end_time;

  // Get start time
  asm volatile ("rdtsc\n\t"    // Returns the time in EDX:EAX.
		"shl $32, %%rdx\n\t"  // Shift the upper bits left.
		"or %%rdx, %0"        // 'Or' in the lower bits.
		: "=a" (start_time)
		:
		: "rdx");

  // could do other work here

  // Get end time
  asm volatile ("rdtsc\n\t"    // Returns the time in EDX:EAX.
		"shl $32, %%rdx\n\t"  // Shift the upper bits left.
		"or %%rdx, %0"        // 'Or' in the lower bits.
		: "=a" (end_time)
		:
		: "rdx");

  __analyzer_eval (start_time == end_time); /* { dg-warning "UNKNOWN" } */

  // Get elapsed time
  return end_time - start_time;
}

static uint64_t get_time (void)
{
  uint64_t result;
  asm volatile ("rdtsc\n\t"    // Returns the time in EDX:EAX.
		"shl $32, %%rdx\n\t"  // Shift the upper bits left.
		"or %%rdx, %0"        // 'Or' in the lower bits.
		: "=a" (result)
		:
		: "rdx");
  return result;
}

uint64_t test_4a (void)
{
  uint64_t start_time, end_time;

  start_time = get_time ();
  // could do other work here
  end_time = get_time ();

  __analyzer_eval (start_time == end_time); /* { dg-warning "UNKNOWN" } */

  // Get elapsed time
  return end_time - start_time;
}

asm ("\t.pushsection .text\n"
     "\t.globl add_asm\n"
     "\t.type add_asm, @function\n"
     "add_asm:\n"
     "\tmovq %rdi, %rax\n"
     "\tadd %rsi, %rax\n"
     "\tret\n"
     "\t.popsection\n");

int test_5 (int count)
{
  asm goto ("dec %0; jb %l[stop]"
	    : "+r" (count)
	    :
	    :
	    : stop);
  return count;
stop:
  return 0;
}
