/* Helper file for i386 platform.  Runtime check for MMX/SSE/SSE2 support.
   Used by 20020523-2.c and i386-sse-6.c, and possibly others.  */
/* Plagarized from 20020523-2.c.  */

#define bit_MMX (1 << 23)
#define bit_SSE (1 << 25)
#define bit_SSE2 (1 << 26)

#ifndef NOINLINE
#define NOINLINE __attribute__ ((noinline))
#endif

unsigned int i386_cpuid (void) NOINLINE;

unsigned int NOINLINE
i386_cpuid (void)
{
  int fl1, fl2;

  /* See if we can use cpuid.  */
  __asm__ ("pushfl; pushfl; popl %0; movl %0,%1; xorl %2,%0;"
	   "pushl %0; popfl; pushfl; popl %0; popfl"
	   : "=&r" (fl1), "=&r" (fl2)
	   : "i" (0x00200000));
  if (((fl1 ^ fl2) & 0x00200000) == 0)
    return (0);

  /* Host supports cpuid.  See if cpuid gives capabilities, try
     CPUID(0).  Preserve %ebx and %ecx; cpuid insn clobbers these, we
     don't need their CPUID values here, and %ebx may be the PIC
     register.  */
  __asm__ ("push %%ecx ; push %%ebx ; cpuid ; pop %%ebx ; pop %%ecx"
	   : "=a" (fl1) : "0" (0) : "edx", "cc");
  if (fl1 == 0)
    return (0);

  /* Invoke CPUID(1), return %edx; caller can examine bits to
     determine what's supported.  */
  __asm__ ("push %%ecx ; push %%ebx ; cpuid ; pop %%ebx ; pop %%ecx" : "=d" (fl2) : "a" (1) : "cc");

  return fl2;
}

