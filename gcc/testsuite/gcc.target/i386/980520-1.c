/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options -O2 } */

int bug(void)
{
        unsigned long a, b;
   
        __asm__(""
                : "=d" (a)
                :
                : "memory");
        __asm__ __volatile__(""
                             :
                             : "g" (b)
                             : "memory");
        return a;
}
