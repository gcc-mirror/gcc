/* { dg-do run } */
/* { dg-additional-options "-std=c99" } */

__attribute__((noinline,noclone))
#ifdef __AVR_TINY__
int func (int a, int b, char c)
#else
int func (long long a, long long b, char c)
#endif
{
    (void) a;
    (void) b;
    return 10 + c;
}

int main (void)
{
    if (func (0, 0, 91) != 101)
        __builtin_abort();
    return 0;
}

