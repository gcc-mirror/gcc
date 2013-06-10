/* { dg-do run { target i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } */

#include <setjmp.h>
#include <signal.h>

#ifndef NULL
#define NULL ((void *)0)
#endif
static jmp_buf segv_jmpbuf;

static void segv_handler(int seg)
{
   __builtin_longjmp(segv_jmpbuf, 1);
}

static int is_addressable(void *p, size_t size)
{
   volatile char * volatile cp = (volatile char *)p;
   volatile int ret;
   struct sigaction sa, origsa;
   sigset_t mask;
   
   sa.sa_handler = segv_handler;
   sa.sa_flags = 0;
   sigfillset(&sa.sa_mask);
   sigaction(SIGSEGV, &sa, &origsa);
   sigprocmask(SIG_SETMASK, NULL, &mask);

   if (__builtin_setjmp(segv_jmpbuf) == 0) {
      while(size--)
	 *cp++;
      ret = 1;
    } else
      ret = 0;

   sigaction(SIGSEGV, &origsa, NULL);
   sigprocmask(SIG_SETMASK, &mask, NULL);

   return ret;
}

int main(int argc, char **argv)
{
   is_addressable(0x0, 1);
   return 0;
}
