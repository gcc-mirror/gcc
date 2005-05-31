/* Test asm clobbers on x86. */

/* { dg-do run { target i?86-*-* x86_64-*-* } } */

extern void abort (void);

int main ()
{
        int i;
        __asm__ ("movl $1,%0\n\txorl %%eax,%%eax" : "=r" (i) : : "eax");
        if (i != 1)
                abort ();
        __asm__ ("movl $1,%0\n\txorl %%ebx,%%ebx" : "=r" (i) : : "ebx");
        if (i != 1)
                abort ();
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
