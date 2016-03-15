/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fsched-pressure -fsel-sched-pipelining -fselective-scheduling" } */

void foo (long long i)
{
   while (i != -1)
     {
	++i;
	 __asm__ ("");
     }
}
