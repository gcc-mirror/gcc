/* { dg-do link } */

typedef __UINT16_TYPE__ u16;
typedef __INT32_TYPE__  T;

#ifdef __OPTIMIZE__

static __inline__ __attribute__((always_inline))
void delay (T x, u16 nops)
{
    do
        __builtin_avr_nops (nops);
    while (--x != -1);
}

#ifdef __AVR_HAVE_JMP_CALL__

void delay_2043 (T x) { delay (x, 2043); }
void delay_2044 (T x) { delay (x, 2044); }
void delay_2045 (T x) { delay (x, 2045); }
void delay_2046 (T x) { delay (x, 2046); }

#endif /* have JUMP, CALL */

void delay_61 (T x) { delay (x, 61); }
void delay_62 (T x) { delay (x, 62); }
void delay_63 (T x) { delay (x, 63); }

#endif /* optimize */

int main (void)
{
    return 0;
}
