/* { dg-do compile { target { ia32 && fstack_protector } } } */
/* { dg-options "-O2 -mincoming-stack-boundary=2 -fstack-protector-all" } */

#include <stdarg.h>

extern int *__errno_location (void);

long
sys_socketcall (int op, ...)
{
  long int res;
  va_list ap;
  va_start (ap, op);
  asm volatile ("push %%ebx; movl %2, %%ebx; int $0x80; pop %%ebx"
  /* { dg-warning "listing the stack pointer register" "" { target *-*-* } .-1 } */
		: "=a" (res) : "0" (102), "ri" (16), "c" (ap) : "memory", "esp");
  if (__builtin_expect (res > 4294963200UL, 0))
    *__errno_location () = -res;
  va_end (ap);
  return res;
}

/* { dg-final { scan-assembler "call\[ \t\]*_?__errno_location" } } */
