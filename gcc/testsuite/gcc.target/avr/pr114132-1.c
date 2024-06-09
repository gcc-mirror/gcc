/* { dg-additional-options "-Os -std=c99" } */

#ifdef __AVR_TINY__
int func (int a, int b, char c)
#else
int func (long long a, long long b, char c)
#endif
{
    (void) a;
    (void) b;

    return c;
}

/* { dg-final { scan-assembler-not "push r28" } } */
