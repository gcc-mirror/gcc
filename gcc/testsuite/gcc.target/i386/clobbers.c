/* Test asm clobbers on x86. */

/* { dg-do run } */

extern void abort (void);

int main ()
{
        int i;
        __asm__ ("movl $1,%0\n\txorl %%eax,%%eax" : "=r" (i) : : "eax");
        if (i != 1)
                abort ();
	/* On darwin you can't call external functions from non-pic code,
	   however, clobbering ebx isn't valid in pic code. Instead of
	   disabling the whole test, just disable the ebx clobbering part.
	   Ditto for any x86 system that is ilp32 && pic.
	*/
#if !(defined (__MACH__))
#if ! defined (__PIC__) || defined (__LP64__)
        __asm__ ("movl $1,%0\n\txorl %%ebx,%%ebx" : "=r" (i) : : "ebx");
        if (i != 1)
                abort ();
#endif /* ! pic || lp64 */
#endif
        __asm__ ("movl $1,%0\n\txorl %%ecx,%%ecx" : "=r" (i) : : "ecx");
        if (i != 1)
                abort ();
        __asm__ ("movl $1,%0\n\txorl %%edx,%%edx" : "=r" (i) : : "edx");
        if (i != 1)
                abort ();
        __asm__ ("movl $1,%0\n\txorl %%esi,%%esi" : "=r" (i) : : "esi");
        if (i != 1)
                abort ();
        __asm__ ("movl $1,%0\n\txorl %%edi,%%edi" : "=r" (i) : : "edi");
        if (i != 1)
                abort ();
        return 0;
}
